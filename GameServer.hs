{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

import           Data.List (intersperse, sortBy)
import           Data.Ord (comparing)
import           Control.Exception
import           Control.Monad (forM_, void, replicateM)
import           Data.Monoid ((<>))
import           Network.WebSockets
import qualified Data.Text as T
import           Data.Text (Text(..))
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map(..))
import           Control.Concurrent
import           Control.Concurrent.Timeout
import           Control.Concurrent.Async
import qualified Database.Redis as Redis
import           System.Clock
import           Problem

type User = (Text, Connection)

data Match = Match {
    players  :: (User, User)
  , score    :: (Int, Int)
  , problems :: [Text]
}

data GameRoom = GameRoom {
    owner        :: User
  , ownerPlaying :: Bool
  , competitors  :: [User]
  , scoreboard   :: Map Text Int
  , questions    :: [Text]
}

data GameServer = GameServer {
    rooms      :: Map Text GameRoom
  , matches    :: Map Text Match
  , clients    :: [User]
}

-- | Time in seconds to answer a question
timeLimit :: Int
timeLimit = 10

encodeMessage :: Text -> Text -> Text
encodeMessage t msg = "{\"type\": \"" <> t <> "\", \"message\": \"" <> msg <> "\"}"

errorMessage :: Text -> Text
errorMessage = encodeMessage "error"

initialServerState :: GameServer
initialServerState = GameServer Map.empty Map.empty []

handleConnection :: Redis.Connection -> MVar GameServer -> PendingConnection -> IO ()
handleConnection redis state pending = do
  connection <- acceptRequest pending 

  uid <- receiveData connection

  usernameMatch <- Redis.runRedis redis $ Redis.get (encodeUtf8 uid)

  case usernameMatch of
    Right (Just bytes) -> let username = decodeUtf8 bytes in flip finally (disconnect username) (newUser state (username, connection))
    Right Nothing         -> sendTextData connection ("Invalid user ID" :: Text)
    Left  _               -> sendTextData connection ("Error processing request" :: Text)

  where
    disconnect username = do
      TIO.putStrLn $ "Disconnecting: " <> username
      modifyMVar_ state $ \s -> return $ s { clients = filter ((/= username) . fst) (clients s) }

newUser :: MVar GameServer -> User -> IO ()
newUser state user@(username, connection) = do
  modifyMVar_ state $ \s -> return $ s { clients = user : (clients s) }
  broadcastGlobalServerMessage state $ username <> " has joined"

  let loop = do
        messageType <- receiveData connection :: IO Text
        case messageType of
          "globalChatMessage" -> receiveData connection >>= broadcastGlobalChatMessage state user
          "roomChatMessage"   -> doRoomMessage state user
          "newRoom"           -> createNewRoom state user
          "joinRoom"          -> joinRoom state user
          _                   -> return ()
        loop

  loop

doRoomMessage :: MVar GameServer -> User -> IO ()
doRoomMessage state user@(username, connection) = do
  roomOwner <- receiveData connection :: IO Text
  room <- ((Map.! roomOwner) . rooms) <$> readMVar state
  message <- receiveData connection :: IO Text
  let recipients = if ownerPlaying room
                   then competitors room
                   else (owner room) : (competitors room)
  forM_ (map snd recipients) $ \c -> do
    sendTextData c $ "{\"type\": \"roomChatMessage\", \"from\": \"" <> username <> "\", \"message\": \"" <> message <> "\"}"

-- | ask user for the name of the room owner, and place them in that room
joinRoom :: MVar GameServer -> User -> IO ()
joinRoom state user@(username, connection) = do
  roomOwner <- receiveData connection :: IO Text
  rs <- rooms <$> readMVar state
  if Map.member roomOwner rs
  then modifyMVar_ state $ \s -> return $ s { rooms = addUser (rooms s) roomOwner }
  else sendTextData connection (errorMessage "No such room!")

  where
    addUser rooms owner = Map.adjust (\g -> g { competitors = user : competitors g, scoreboard = Map.insert username 0 (scoreboard g) }) owner rooms
    

-- | does not currently check for already existing rooms by this user
createNewRoom state user@(username, connection) = do
  ownerPlays <- (== ("true" :: Text)) <$> receiveData connection
  let competitors = if ownerPlays then [user] else []
  let room = GameRoom user ownerPlays competitors Map.empty []
  modifyMVar_ state $ \s -> return $ s { rooms = Map.insert username room (rooms s) }

  -- | block until another message is received, and then begin the game
  void $ (receiveData connection :: IO Text)

  startRoom state user

broadcastGlobalChatMessage :: MVar GameServer -> User -> Text -> IO ()
broadcastGlobalChatMessage state from message = do
  users <- (map snd . clients) <$> readMVar state
  forM_ users $ \c -> do
    sendTextData c $ "{\"type\": \"globalChatMessage\", \"from\": \"" <> (fst from) <> "\", \"message\": \"" <> message <> "\"}"

broadcastGlobalServerMessage :: MVar GameServer -> Text -> IO ()
broadcastGlobalServerMessage state message = do
  users <- (map snd . clients) <$> readMVar state
  forM_ users $ \c -> do
    sendTextData c $ "{\"type\": \"globalServerMessage\", \"message\": \"" <> message <> "\"}"

startRoom :: MVar GameServer -> User -> IO ()
startRoom state owner = do
  ps <- (competitors . (Map.! (fst owner)) . rooms) <$> readMVar state

  questions <- replicateM 5 (randomProblem Hard)

  putStrLn "ABOUT TO START"

  forM_ questions $ \q -> do
    broadcastQuestion ps q
    putStrLn "SENT QUESTION"
    answers <- mapConcurrently getAnswer ps
    putStrLn "GOT ANSWERS"
    updateScores q answers >>= broadcastScoreboard ps

  where
    updateScores q answers = do
      modifyMVar_ state $ \s -> do
        let room = (rooms s) Map.! (fst owner)
        let newRoom = room { scoreboard = updateScoreboard (scoreboard room) answers }
        return $ s { rooms = Map.adjust (const newRoom) (fst owner) (rooms s) }
      (scoreboard . (Map.! (fst owner)) . rooms) <$> readMVar state
      where
        updateScoreboard board [] = board
        updateScoreboard board (a:as) = case a of
          (username, answer, elapsed) | elapsed < timeLimit -> updateScoreboard (Map.adjust (scoreModifier answer) username board) as
                                      | otherwise -> updateScoreboard board as
        scoreModifier ans
          | ans == answer q = (+1)
          | otherwise       = id

    broadcastScoreboard ps board = forM_ ps $ \p -> sendTextData (snd p) (encodeMessage "scoreboardUpdate" (encodeScoreboard board))
    broadcastQuestion ps q = forM_ ps $ \p -> sendTextData (snd p) $ encodeMessage "newQuestion" (string q)
    encodeScoreboard board = "{" <> mconcat (intersperse "," ["\"" <> username <> "\": \"" <> (T.pack . show) score <> "\"" | (username, score) <- (sortBy (comparing snd) (Map.toList board))]) <> "}"
      
getAnswer :: User -> IO (Text, Text, Int)
getAnswer (username, connection) = do
  start  <- getTime Monotonic
  answer <- receiveData connection
  end <- getTime Monotonic
  let elapsed = fromIntegral $ sec end - sec start
  return (username, answer, elapsed)

main :: IO ()
main = do
  state <- newMVar initialServerState
  redis <- Redis.connect Redis.defaultConnectInfo
  runServer "0.0.0.0" 8000 (handleConnection redis state)

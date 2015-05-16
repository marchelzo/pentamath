{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

import           Data.List (intersperse, sortBy)
import           Data.Ord (comparing)
import           Control.Exception
import           Control.Monad (forM_, void, replicateM, when)
import           Data.Monoid ((<>))
import           Network.WebSockets
import qualified Data.Text as T
import           Data.Text (Text(..))
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map(..))
import           Control.Concurrent
import           Control.Concurrent.Async
import qualified Database.Redis as Redis
import           System.Clock
import           Problem
import           Data.Set (Set)
import qualified Data.Set as Set

type User = (Text, Connection)

data GameRoom = GameRoom {
    owner        :: User
  , ownerPlaying :: Bool
  , competitors  :: [User]
  , scoreboard   :: Map Text Int
  , questions    :: [Text]
}

data GameServer = GameServer {
    rooms      :: Map Text GameRoom
  , clients    :: [User]
  , connected  :: Set Text
  , challenges :: Map Text Text
}

-- | Time in seconds to answer a question
timeLimit :: Int
timeLimit = 10

encodeMessage :: Text -> Text -> Text
encodeMessage t msg = "{\"type\": \"" <> t <> "\", \"message\": \"" <> msg <> "\"}"

encodeMessage' :: Text -> Text -> Text
encodeMessage' t msg = "{\"type\": \"" <> t <> "\", \"message\": " <> msg <> "}"

errorMessage :: Text -> Text
errorMessage = encodeMessage "error"

initialServerState :: GameServer
initialServerState = GameServer Map.empty [] Set.empty Map.empty

handleConnection :: Redis.Connection -> MVar GameServer -> PendingConnection -> IO ()
handleConnection redis state pending = do
  connection <- acceptRequest pending 

  uid <- receiveData connection

  usernameMatch <- Redis.runRedis redis $ Redis.get (encodeUtf8 uid)

  case usernameMatch of
    Right (Just bytes) -> let username = decodeUtf8 bytes in flip finally (disconnect username) (newUser state (username, connection))
    Right Nothing         -> sendTextData connection $ errorMessage "noToken"
    Left  _               -> sendTextData connection $ errorMessage "serverError"

  where
    disconnect username = do
      -- | TIO.putStrLn $ "Disconnecting: " <> username
      modifyMVar_ state $ \s -> return $ s { clients = filter ((/= username) . fst) (clients s) }
      modifyMVar_ state $ \s -> return $ s { connected = Set.delete username (connected s) }

dispatch :: MVar GameServer -> User -> Text -> IO ()
dispatch state user@(username, connection) messageType =
  case messageType of
    "globalChatMessage" -> receiveData connection >>= broadcastGlobalChatMessage state user
    "roomChatMessage"   -> doRoomMessage state user
    "newRoom"           -> createNewRoom state user
    "joinRoom"          -> joinRoom state user
    "practiceProblems"  -> sendPracticeProblems user
    "initiateChallenge" -> initiateChallenge state user
    "acceptChallenge"   -> acceptChallenge state user
    _                   -> return ()

initiateChallenge state user@(username, connection) = do
  opponent <- receiveData connection
  oppConnection <- (lookup opponent . clients) <$> readMVar state
  TIO.putStrLn $ "SEARCHED FOR OPPONENT " <> opponent
  case oppConnection of
    Nothing -> sendTextData connection $ errorMessage "noOpponent"
    Just c  -> sendTextData c $ encodeMessage "challengeRequest" username

acceptChallenge state user@(username, connection) = do
  opponent <- receiveData connection
  oppConnection <- (lookup opponent . clients) <$> readMVar state
  case oppConnection of
    Nothing -> return ()
    Just c -> do
      makeChallengeEntry username opponent
      ready <- challengeReady username opponent
      if ready
      then doChallenge (opponent, c) user
      else notifyAcceptance c username
  where
    makeChallengeEntry u o = modifyMVar_ state $ \s -> return $ s { challenges = Map.insert u o (challenges s) }
    challengeReady u o = do
      cs <- challenges <$> readMVar state
      return $ Map.lookup u cs == Just o && Map.lookup o cs == Just u
    notifyAcceptance c u = sendTextData c $ encodeMessage "challengeAccepted" u
        
doChallenge :: User -> User -> IO ()
doChallenge u1@(name1, c1) u2@(name2, c2) = do
  sendTextData c1 $ encodeMessage "challengeBeginning" ""
  sendTextData c2 $ encodeMessage "challengeBeginning" ""
  threadDelay $ 3 * 1000000
  u1Status <- async (receiveData c1) :: IO (Async Text)
  u2Status <- async (receiveData c2) :: IO (Async Text)
  question1 <- randomProblem Medium
  question2 <- randomProblem Medium
  let loop u1s u2s score1 score2 q1 q2 = do
      if score1 == 20 || score2 == 20
      then do
        if score1 == 20
        then winnerLoser u1 u2
        else winnerLoser u2 u1
      else do
        sendTextData c1 $ encodeMessage "newChallengeQuestion" (string q1)
        sendTextData c2 $ encodeMessage "newChallengeQuestion" (string q2)
        sendTextData c1 $ encodeMessage "opponentQuestion" (string q2)
        sendTextData c2 $ encodeMessage "opponentQuestion" (string q1)
        response <- waitEither u1s u2s
        case response of
          Left ans -> do
            let bonus = if ans == (answer q1) then 1 else 0 :: Int
            when (bonus == 1) $ sendTextData c2 $ encodeMessage "opponentScore" ""
            when (bonus == 1) $ sendTextData c1 $ encodeMessage "youScore" ""
            newU1s <- async $ receiveData c1
            newQ1 <- randomProblem Medium
            loop newU1s u2s (score1 + bonus) score2 newQ1 q2
          Right ans -> do
            let bonus = if ans == (answer q2) then 1 else 0 :: Int
            when (bonus == 1) $ sendTextData c1 $ encodeMessage "opponentScore" ""
            when (bonus == 1) $ sendTextData c2 $ encodeMessage "youScore" ""
            newU2s <- async $ receiveData c2
            newQ2 <- randomProblem Medium
            loop u1s newU2s score1 (score2 + bonus) q1 newQ2

  loop u1Status u2Status 0 0 question1 question2

  where
    winnerLoser win lose = do
      sendTextData (snd win) $ encodeMessage "challengeComplete" "win"
      sendTextData (snd lose) $ encodeMessage "challengeComplete" "lose"

newUser :: MVar GameServer -> User -> IO ()
newUser state user@(username, connection) = do
  modifyMVar_ state $ \s -> return $ s { clients = user : (clients s) }
  alreadyHere <- (Set.member username . connected) <$> readMVar state
  when (not alreadyHere) $ broadcastGlobalServerMessage state $ username <> " has joined"
  modifyMVar_ state $ \s -> return $ s { connected = Set.insert username (connected s) }

  let loop = do
        messageType <- receiveData connection :: IO Text
        dispatch state user messageType
        when (messageType /= "stopLoop") loop

  loop

sendPracticeProblems :: User -> IO ()
sendPracticeProblems (_, connection) = do
  problems <- replicateM 5 (randomProblem VeryEasy)
  let problemArray = "[" <> mconcat (intersperse "," ["\"" <> p <> "\"" | p <- (map string problems)]) <> "]"
  let answerArray = "[" <> mconcat (intersperse "," ["\"" <> a <> "\"" | a <- (map answer problems)]) <> "]"
  let response = "{\"type\": \"practiceProblems\", \"problems\": " <> problemArray <> ", \"answers\": " <> answerArray <> "}"
  sendTextData connection response

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
  -- | TIO.putStrLn $ "Request to join: " <> roomOwner
  rs <- rooms <$> readMVar state
  if Map.member roomOwner rs
  then do
    others <- (competitors . (Map.! roomOwner) . rooms) <$> readMVar state
    forM_ (map snd others) $ \c -> sendTextData c $ encodeMessage "userJoinedRoom" username
    let message = "[" <> mconcat (intersperse "," (map ((\t -> "\"" <> t <> "\"") . fst) others)) <> "]"
    sendTextData connection $ encodeMessage' "userList" message
    modifyMVar_ state $ \s -> return $ s { rooms = addUser (rooms s) roomOwner }
  else sendTextData connection (errorMessage "noRoom")

  threadDelay 10000000000

  where
    addUser rooms owner = Map.adjust (\g -> g { competitors = user : competitors g, scoreboard = Map.insert username 0 (scoreboard g) }) owner rooms
    

-- | does not currently check for already existing rooms by this user
createNewRoom state user@(username, connection) = do
  ownerPlays <- (== ("true" :: Text)) <$> receiveData connection
  difficulty <- (receiveData connection :: IO Text) >>= \d -> return $ case d of
    "easy" -> VeryEasy
    "medium" -> Hard
    "hard" -> Insane
  let competitors = if ownerPlays then [user] else []
  let scoreboard = if ownerPlays then Map.singleton username 0 else Map.empty
  let room = GameRoom user ownerPlays competitors scoreboard []
  modifyMVar_ state $ \s -> return $ s { rooms = Map.insert username room (rooms s) }

  -- | broadcast the new room
  cs <- (map snd . clients) <$> readMVar state
  forM_ cs $ \c -> sendTextData c (encodeMessage "newRoom" username)

  -- | continue dispatching chat messages, etc. but check for 'start'
  let loop = do
      msgType <- receiveData connection :: IO Text
      when (msgType /= "start") $ dispatch state user msgType >> loop

  loop

  startRoom difficulty state user

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

startRoom :: Difficulty -> MVar GameServer -> User -> IO ()
startRoom difficulty state owner = do
  room <- ((Map.! (fst owner)) . rooms) <$> readMVar state
  let ps = competitors room

  questions <- replicateM 5 (randomProblem difficulty)

  -- | putStrLn "ABOUT TO START"

  -- | putStrLn "PLAYERS:"
  -- | mapM_ TIO.putStrLn (map fst ps)

  forM_ questions $ \q -> do
    broadcastQuestion ps q
    -- | putStrLn "SENT QUESTION"
    answers <- mapConcurrently getAnswer ps

    forM_ (map snd ps) $ \c -> sendTextData c $ encodeMessage "answer" (answer q)
    -- | putStrLn "GOT ANSWERS"
    -- | mapM_ TIO.putStrLn (map (\(a,b,c) -> b) answers)
    let scoreboardRecipients = if ownerPlaying room then ps else (owner : ps)
    updateScores q answers >>= broadcastScoreboard scoreboardRecipients

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

    broadcastScoreboard ps board = forM_ ps $ \p -> sendTextData (snd p) (encodeMessage' "scoreboardUpdate" (encodeScoreboard board))
    broadcastQuestion ps q = forM_ ps $ \p -> sendTextData (snd p) $ encodeMessage "newQuestion" (string q)
    encodeScoreboard board = "{" <> mconcat (intersperse "," ["\"" <> username <> "\": \"" <> (T.pack . show) score <> "\"" | (username, score) <- (sortBy (comparing snd) (Map.toList board))]) <> "}"
      
getAnswer :: User -> IO (Text, Text, Int)
getAnswer (username, connection) = do
  -- | TIO.putStrLn $ "Getting answer from: " <> username
  start  <- getTime Monotonic
  answer <- receiveData connection
  -- | TIO.putStrLn $ "Got answer from: " <> username <> " ---- " <> answer
  end <- getTime Monotonic
  let elapsed = fromIntegral $ sec end - sec start
  return (username, answer, elapsed)

main :: IO ()
main = do
  state <- newMVar initialServerState
  redis <- Redis.connect Redis.defaultConnectInfo
  runServer "0.0.0.0" 8000 (handleConnection redis state)

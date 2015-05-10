{-# LANGUAGE OverloadedStrings #-}

import Problem
import Control.Monad (void)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Text.Encoding (decodeUtf8)
import Network.WebSockets
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Database.Redis as Redis
import Control.Concurrent
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map(..))
import System.IO

receiveTextMessage c = (\(Text s) -> s) <$> receiveDataMessage c

type User = (String, Connection)
type Game = ([User], [User])
type ServerState = Map Int Game

initialServerState :: ServerState
initialServerState = Map.empty

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  state <- newMVar initialServerState
  runServer "127.0.0.1" 8000 (handleConnection state)

-- | Split ByteString at the first and second space.
-- | Request messages are of the form
-- | `username action <game_id>?`
-- | so there should be at most one space.
parseRequestMessage :: LBS.ByteString -> (LBS.ByteString, LBS.ByteString, LBS.ByteString)
parseRequestMessage s = let (a, b) = LBS.splitAt 10 s
                            (c, d) = LBS.splitAt 10 b
                        in (a, b, c)

handleConnection state pending = do
  connection <- acceptRequest pending
  (name, action, idString) <- parseRequestMessage <$> receiveTextMessage connection
  let user = (LBSC.unpack name, connection)
  let id   = (read . LBSC.unpack) idString
  case action of
    "create" -> newGame state user
    "play"   -> joinGame id state user
    _        -> sendTextData connection ("invalid" :: Text)

createGame :: MVar ServerState -> User -> IO Int
createGame state user = do
  s <- takeMVar state
  let id = Map.size s
  let game = ([user], [])
  putMVar state (Map.insert id game s)
  return id

newGame :: MVar ServerState -> User -> IO ()
newGame state user = do
  id <- createGame state user
  print id -- | debug
   
  let loop = do
      action <- receiveTextMessage (snd user)
      case action of
        "update" -> sendUserList state id (snd user) >> loop
        "start"  -> startGame id

  loop

sendUserList :: MVar ServerState -> Int -> Connection -> IO ()
sendUserList state id c = do
  (spectators, players) <- (Map.! id) <$> readMVar state
  let messageString = show [map fst spectators, map fst players]
  sendTextData c (T.pack messageString)


joinGame :: Int
            -> MVar ServerState
            -> User
            -> IO ()
joinGame id state user = do
  s <- takeMVar state
  putMVar state (Map.update addUser id s)
  where
    addUser (ss, ps) = Just (ss, user:ps)

startGame :: Int -> IO ()
startGame = undefined

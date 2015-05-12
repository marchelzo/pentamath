{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception (finally)
import           Control.Monad (forM_)
import           Data.Monoid ((<>))
import           Network.WebSockets
import qualified Data.Text as T
import           Data.Text (Text(..))
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map(..))
import           Control.Concurrent
import qualified Database.Redis as Redis

type User = (Text, Connection)

data Match = Match {
    players  :: (User, User)
  , score    :: (Int, Int)
  , problems :: [Text]
}

data GameRoom = GameRoom {
    owner       :: User
  , competitors :: [User]
  , scoreboard  :: Map Text Int
  , questions   :: [Text]
}

data GameServer = GameServer {
    rooms      :: [GameRoom]
  , matches    :: [Match]
  , clients    :: [User]
}


initialServerState :: GameServer
initialServerState = GameServer [] [] []

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
    disconnect username = modifyMVar_ state $ \s -> return $ s { clients = filter ((/= username) . fst) (clients s) }

newUser :: MVar GameServer -> User -> IO ()
newUser state user@(username, connection) = do
  modifyMVar_ state $ \s -> return $ s { clients = user : (clients s) }
  broadcastGlobalServerMessage state $ "Server: " <> username <> " has joined"

  let loop = do
    message <- receiveData connection
    broadcastGlobalChatMessage state user message
    loop

  loop

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
    
  

main :: IO ()
main = do
  state <- newMVar initialServerState
  redis <- Redis.connect Redis.defaultConnectInfo
  runServer "0.0.0.0" 8000 (handleConnection redis state)

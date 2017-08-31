{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Lib
  ( runServer
  ) where

import           Control.Concurrent.STM      (STM, TVar, atomically, modifyTVar,
                                              newTVar, readTVar, writeTVar)
import           Control.Lens                (assign, at, makeLenses, set)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Proxy                  (Proxy (Proxy))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time                   (UTCTime, addUTCTime, diffUTCTime,
                                              getCurrentTime)
import           Data.UUID                   (UUID)
import           GHC.Generics                (Generic)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant
import           Servant.API
import           Servant.Server              ((:~>) (NT))

type UserID = Text

data Object
  = Rock
  | Paper
  | Scissors
  deriving (Show, Ord, Eq, FromJSON, ToJSON, Generic)

data Game = Game
  { _votes         :: Map UserID Object
  , _scores        :: Map UserID Int
  , _nextJudgement :: UTCTime
  } deriving (Show, Eq, FromJSON, ToJSON, Generic)

makeLenses ''Game

data GameView = GameView
  { allScores  :: Map UserID Int
  , timeLeftMs :: Int
  } deriving (Show, Eq, FromJSON, ToJSON, Generic)

data Vote = Vote
  { object :: Object
  , userID :: UserID
  } deriving (Show, Ord, Eq, FromJSON, ToJSON, Generic)

------------------------------------------------------------
type API = "api" :> "state" :> Get '[ JSON] GameView :<|> "api" :> "vote" :> ReqBody '[ JSON] Vote :> Post '[ JSON] ()

api :: Proxy API
api = Proxy

-- readerToHandler' :: STM Game -> ReaderT (STM Game) IO a -> Handler a
-- readerToHandler' stm r = liftIO $ runReaderT r stm
-- readerToHandler :: STM Game -> ReaderT (STM Game) IO :~> Handler
-- readerToHandler stm = NT (readerToHandler' stm)
server :: TVar Game -> Server API
server theGame = getGameView theGame :<|> processVote theGame

getGameView :: TVar Game -> Handler GameView
getGameView theGame = do
  now <- liftIO $ getCurrentTime
  gameState <- liftIO $ atomically $ readTVar theGame
  pure $ toView now gameState

-- do
--   currentGame <- atomically $ readTVar
--   pure $ toView currentGame
toView :: UTCTime -> Game -> GameView
toView now (Game {..}) = GameView {..}
  where
    allScores = _scores
    timeLeftMs = round $ diffUTCTime _nextJudgement now

processVote :: TVar Game -> Vote -> Handler ()
processVote theGame vote = do
  liftIO $ atomically $ modifyTVar theGame (registerVote vote)

registerVote :: Vote -> Game -> Game
registerVote (Vote {..}) = set (votes . at userID) (Just object)

app :: TVar Game -> Application
app theGame = serve api (server theGame)

initialGame :: UTCTime -> Game
initialGame = Game Map.empty Map.empty

runServer :: IO ()
runServer = do
  now <- getCurrentTime
  let startTime = nextGame now
  theGame <- atomically $ newTVar $ initialGame startTime
  putStrLn "Starting"
  run 8080 $ simpleCors $ app theGame

nextGame :: UTCTime -> UTCTime
nextGame = addUTCTime (fromIntegral 10)

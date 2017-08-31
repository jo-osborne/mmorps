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

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM      (STM, TVar, atomically, modifyTVar,
                                              newTVar, readTVar, writeTVar)
import           Control.Lens                (assign, at, makeLenses, set, view)
import           Control.Monad               (forever)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe
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

server :: TVar Game -> Server API
server theGame = getGameView theGame :<|> processVote theGame

getGameView :: TVar Game -> Handler GameView
getGameView theGame = do
  now <- liftIO $ getCurrentTime
  gameState <- liftIO $ atomically $ readTVar theGame
  pure $ toView now gameState

toView :: UTCTime -> Game -> GameView
toView now (Game {..}) = GameView {..}
  where
    allScores = _scores
    timeLeftMs = round $ (*) 1000 $ toRational $ diffUTCTime _nextJudgement now

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
  _ <- forkIO $ judge theGame
  putStrLn "Starting"
  run 8080 $ simpleCors $ app theGame

judge :: TVar Game -> IO ()
judge theGame =
  forever $ do
    putStrLn "Sleeping"
    threadDelay (1 * 1000 * 1000)
    putStrLn "Judging"
    now <- getCurrentTime
    print now
    atomically $ do
      currentGame <- readTVar theGame
      if (view nextJudgement currentGame) < now
        then modifyTVar theGame $ closeGame now
        else pure ()
    newGame <- atomically $ readTVar theGame
    print newGame

closeGame :: UTCTime -> Game -> Game
closeGame now theGame =
  set scores newScores $ set nextJudgement (nextGame now) theGame
  where
    frequencies :: Map Object Int
    frequencies = Map.foldl doTally Map.empty (view votes theGame)
    doTally :: Map Object Int -> Object -> Map Object Int
    doTally tally object = Map.alter (inc 1) object tally
    inc x Nothing  = Just x
    inc x (Just n) = Just (n + x)
    newScores :: Map UserID Int
    newScores =
      Map.foldlWithKey addScore (view scores theGame) (view votes theGame)
    addScore currentScores userID object =
      let delta = sum $ map (f object) [Rock, Paper, Scissors]
      in Map.alter (inc delta) userID currentScores
    f :: Object -> Object -> Int
    f userObject otherObject =
      let n = score userObject otherObject
      in n * (fromMaybe 0 (Map.lookup otherObject frequencies))

score :: Object -> Object -> Int
score Rock Paper     = -1
score Paper Rock     = 1
score Scissors Rock  = -1
score Rock Scissors  = 1
score Paper Scissors = -1
score Scissors Paper = 1

nextGame :: UTCTime -> UTCTime
nextGame = addUTCTime (fromIntegral 3)

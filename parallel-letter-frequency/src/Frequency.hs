module Frequency (frequency) where

import Data.Map (Map, empty, insertWith, unionsWith, unionWith)
import qualified Data.Text as T (Text, foldl)
import Data.Char (toLower, isAlpha)

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

worker :: TQueue T.Text -> TQueue (Map Char Int) -> IO ()
worker jobs results = forever $ do
    text <- atomically $ readTQueue jobs
    atomically $ writeTQueue results (frequencyOne text)

frequency :: Int -> [T.Text] -> IO (Map Char Int)
frequency nWorkers texts = do
    jobs <- atomically newTQueue
    results <- atomically newTQueue

    forM_ [1..nWorkers] $ \_ ->
        forkIO $ worker jobs results

    forM_ texts $ atomically . writeTQueue jobs

    mapFromTQueue results texts

mapFromTQueue :: TQueue (Map Char Int) -> [a] -> IO (Map Char Int)
mapFromTQueue q = foldl r (return empty)
    where
        r :: IO (Map Char Int) -> a -> IO (Map Char Int)
        r f _ = do 
            b <- atomically $ readTQueue q
            unionWith (+) b <$> f

frequencyOne :: T.Text -> Map Char Int
frequencyOne = T.foldl compute empty
    where
        compute map char
            | isAlpha char = insertWith func (toLower char) 1 map
            | otherwise = map
        func new old = old+new

module Main where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as S

import Data.Time.Clock

import Control.Monad
import Control.Exception
import Control.DeepSeq

timing :: IO a -> IO a
timing mx = do
  t0 <- getCurrentTime
  x <- mx
  t1 <- getCurrentTime
  putStrLn $ "took " ++ show (t1 `diffUTCTime` t0)
  return x

n = 1000 * 1000
k = 1000

baseVector :: S.Vector Int
baseVector = G.enumFromN 1 n

dropWhile_ :: (G.Vector v a) => (a -> Bool) -> v a -> v a
dropWhile_ pred vec = case G.findIndex pred vec of
  Nothing -> G.empty
  Just n  -> G.drop n vec

main :: IO ()
main = do
  timing $ forM_ [1..k] $ \i -> do
    evaluate . force $ dropWhile_ (<i*i) baseVector

  timing $ forM_ [1..k] $ \i -> do
    evaluate . force $ G.dropWhile (<i*i) baseVector


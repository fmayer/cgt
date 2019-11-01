module Main where

import Lib

import qualified Data.Vector as V
import Control.Arrow
import Control.Lens

main :: IO ()
main = do
  csvVec <- fmap (right usdToGbp) (parseFX "eurofxref-hist.csv")
  trades <- parseTrades "goog.csv"
  case csvVec of
    Left err -> putStrLn err
    Right v -> putStrLn $ show v
-- V.forM_ v $ \ p ->
 --       putStrLn $ show (date p) ++ " gbp " ++ show (gbp p) ++ " usd " ++ show (usd p)

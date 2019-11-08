-- Copyright (C) 2019 Florian Mayer
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
module Main where

import           Lib

import           Control.Arrow
import           Control.Monad
import           Data.Decimal
import           Data.HashMap.Lazy
import           Data.Time.Calendar
import           Data.Tuple
import qualified Data.Vector        as V

main :: IO ()
main = do
  csvVec <- parseFX "eurofxref-hist.csv"
  let table = csvVec >>= return . usdToGbp
  trades <- parseTrades "goog.csv"
  let pounds = join $ return getPoundValues `ap` table `ap` trades
  let total = pounds >>= return . (foldl (+) 0)
  let cnt = trades >>= return . (foldl (+) 0) . fmap amount
  let totalcnt = return (,) `ap` total `ap` cnt
  case totalcnt of
    Right (v, n) ->
      putStrLn $
      (show n) ++ " shares" ++ " at " ++ (show $ roundTo 4 v) ++ " GBP"
    Left x -> putStrLn $ "OHNO! " ++ x

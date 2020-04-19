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

import           Control.Monad
import           Data.Decimal
import           Data.Map
import qualified Data.Vector

groupBySymbol x =  fromListWith (++) [(Lib.symbol t, [t]) | t <- x]

handleTrades table symbol t =  do
  let sortedT = sortBy Lib.date t
  let trades = pure $ Data.Vector.fromList t
  let pounds = join $ getPoundValues <$> table <*> trades
  let total = sum <$> pounds
  let cnt = sum . fmap amount <$> trades
  let totalcnt = (,) <$> total <*> cnt
  case totalcnt of
    Right (v, n) ->
      putStrLn $
      (show n) ++ " " ++ symbol ++ " shares" ++ " at " ++ (show $ roundTo 4 v) ++ " GBP"
    Left x -> putStrLn $ "OHNO! " ++ x


main :: IO ()
main = do
  csvVec <- parseFX "eurofxref-hist.csv"
  let table = usdToGbp <$> csvVec
  trades <- parseTrades "goog.csv"
  let tradesBySymbol = groupBySymbol <$> (Data.Vector.toList <$> trades)
  let ms = mapWithKey (handleTrades table) <$> tradesBySymbol
  case sequence_ <$> (elems <$> ms) of
    Right m -> m
    Left x  -> putStrLn $ "OHNO! " ++ x

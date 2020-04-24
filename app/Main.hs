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
import           Data.List
import           Data.Time.Calendar
import qualified Data.Vector        as V

getCostBasis :: HashMap Day Decimal -> String -> [Trade] -> Either String [CapitalGains]
getCostBasis fxTable symbol t =  do
  poundTrades <-  getPoundValues fxTable t
  let sortedT = sortOn (tradeDate &&& actionOrder . action) poundTrades
  pure $ handleTrades sortedT


main :: IO ()
main = do
  csvVec <- parseFX "eurofxref-hist.csv"
  let table = usdToGbp <$> csvVec
  trades <- parseTrades "goog.csv"
  let tradesBySymbol = groupBySymbol <$> (V.toList <$> trades)
  let ms = mapWithKey <$> (getCostBasis <$> table) <*> tradesBySymbol
  let msE = elems <$> ms
  case sequence $ sequence $ msE of
    Right m -> putStrLn $ show m
    Left x  -> putStrLn $ "OHNO! " ++ x

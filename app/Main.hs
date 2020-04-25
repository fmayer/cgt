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
import           Data.Semigroup      ((<>))
import           Data.Time.Calendar
import qualified Data.Vector         as V
import           Options.Applicative

getCostBasis :: HashMap Day Decimal -> String -> [Trade] -> Either String [CapitalGains]
getCostBasis fxTable symbol t =  do
  poundTrades <-  getPoundValues fxTable t
  let sortedT = sortOn (tradeDate &&& actionOrder . Lib.action) poundTrades
  pure $ handleTrades sortedT

data Args = Args
  { fxCSV     :: String
  , tradeCSVs :: [String]
  }

args :: Parser Args
args = Args
  <$> strOption ( long "fx" <> metavar "FILE" <> Options.Applicative.action "file" )
  <*> many (argument str (Options.Applicative.action "file"))

opts :: ParserInfo Args
opts = info (args <**> helper)
  ( fullDesc
  <> progDesc "Calculate capital gains tax"
  <> header "cgt - a capital gains tax calculator" )

showCG :: (String, Either String [CapitalGains]) -> IO ()
showCG (symbol, Left err) = putStrLn $ "failed to calculate for " ++ symbol ++ ": " ++ err
showCG (symbol, Right gains) = putStrLn $ symbol ++ ": " ++ (show gains)

main :: IO ()
main = do
  a <- execParser opts
  csvVec <- parseFX $ fxCSV a
  let table = usdToGbp <$> csvVec
  trades <- sequence $ Data.List.map parseTrades (tradeCSVs a)
  let tradesBySymbol = groupBySymbol <$> (concat <$> (sequence trades))
  let ms = mapWithKey <$> (getCostBasis <$> table) <*> tradesBySymbol
  let msL = toList <$> ms
  case msL of
    Right m -> sequence_ $ Data.List.map showCG m
    Left x  -> putStrLn $ "OHNO! " ++ x

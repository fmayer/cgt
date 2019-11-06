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
import qualified Data.Vector        as V

mLookup :: HashMap Day b -> Day -> Either String b
mLookup h k =
  case Data.HashMap.Lazy.lookup k h of
    Just x  -> return x
    Nothing -> fail $ "failed to lookup " ++ (show k)

main :: IO ()
main = do
  csvVec <- parseFX "eurofxref-hist.csv"
  let table = csvVec >>= return . usdToGbp
  trades <- parseTrades "goog.csv"
  let rates =
        join $
        (table >>= return . mapM . mLookup) `ap`
        (trades >>= return . fmap tradeDate)
  let dollars =
        trades >>= return . fmap (\p -> fmv p * (fromIntegral $ amount p))
  let pounds = return (V.zipWith (*)) `ap` rates `ap` dollars
  let total = pounds >>= return . (foldl (+) 0)
  case total of
    Right x -> putStrLn $ show $ roundTo 4 x
    Left x  -> putStrLn x

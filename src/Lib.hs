{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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

module Lib
  ( parseFX,
    parseTrades,
    date,
    getPoundValues,
    gbp,
    usd,
    usdToGbp,
    maybeParseAmericanDate,
    maybeParseISODate,
    groupBySymbol,
    actionOrder,
    takeFromCostBasis,
    addCostBases,
    CostBasis (..),
    Trade (..),
    Action (..),
    CapitalGains (..),
    handleTrades,
    getTaxYear,
  )
where

import qualified Control.Applicative as A
import Control.Arrow
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Decimal
import Data.HashMap.Lazy as HM
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Text.Encoding
import Data.Time.Calendar
import Data.Time.Format
import qualified Data.Vector as V
import GHC.Generics (Generic)

data FXRow
  = FXRow
      { date :: !Day,
        gbp :: !Decimal,
        usd :: !Decimal
      }
  deriving (Generic, Show)

data Action
  = Buy
  | Sell
  deriving (Show)

data Trade
  = Trade
      { symbol :: !String,
        action :: !Action,
        tradeDate :: !Day,
        amount :: !Decimal,
        fmv :: !Decimal
      }
  deriving (Generic, Show)

data CostBasis
  = CostBasis
      { shares :: !Decimal,
        totalPrice :: !Decimal
      }
  deriving (Generic, Show, Eq)

data CapitalGains
  = CapitalGains
      { costBasis :: !CostBasis,
        gains :: !Decimal,
        cgDate :: !Day
      }
  deriving (Generic, Show, Eq)

newtype Dollars = Dollars Decimal

instance FromNamedRecord FXRow where
  parseNamedRecord r = FXRow <$> r .: "Date" <*> r .: "GBP" <*> r .: "USD"

instance FromNamedRecord Trade where
  parseNamedRecord r =
    Trade
      <$> r .: "Symbol"
      <*> r .: "Action"
      <*> r .: "Date"
      <*> r .: "Quantity"
      <*> (dollarToDecimal <$> r .: "Price")

instance Hashable Day where
  hashWithSalt s a = hashWithSalt s $ showGregorian a

instance FromField Action where
  parseField "Buy" = A.pure Buy
  parseField "Deposit" = A.pure Buy
  parseField "Sell" = A.pure Sell
  parseField s = fail $ "invalid action: " ++ (unpack . decodeUtf8 $ s)

instance FromField Decimal where
  parseField s =
    case reads decodedS of
      [(x, "")] -> A.pure x
      _ -> fail $ "invalid decimal: " ++ decodedS
    where
      decodedS = unpack . decodeUtf8 $ s

instance FromField Day where
  parseField s =
    case maybeParseDate decodedS of
      Just d -> A.pure d
      Nothing -> fail $ "invalid day: " ++ decodedS
    where
      decodedS = unpack . decodeUtf8 $ s

instance FromField Dollars where
  parseField s =
    case BS8.uncons s of
      Just (c, rest) ->
        case c of
          '$' ->
            case reads . unpack . decodeUtf8 $ BS8.filter ((/=) ',') rest of
              [(x, "")] -> A.pure (Dollars x)
              _ -> A.empty
          _ -> A.empty
      _ -> A.empty

-- UK tax year runs from Apr 6 to Apr 5.
getTaxYear :: Day -> Integer
getTaxYear d
  | month < 4 || month == 4 && day < 6 = year - 1
  | otherwise = year
  where
    (year, month, day) = toGregorian d

dollarToDecimal :: Dollars -> Decimal
dollarToDecimal (Dollars d) = d

totalValue :: Trade -> Decimal
totalValue t = (amount t) * fmv t

addToCostBasis :: Trade -> CostBasis -> CostBasis
addToCostBasis t c = CostBasis (amount t + shares c) (totalValue t + totalPrice c)

addCostBases :: CostBasis -> CostBasis -> CostBasis
addCostBases (CostBasis s1 t1) (CostBasis s2 t2) = CostBasis (s1 + s2) (t1 + t2)

takeFromCostBasis :: CostBasis -> Decimal -> (CostBasis, CostBasis)
takeFromCostBasis c 0 = ((CostBasis 0 0), c)
takeFromCostBasis c _ | shares c <= 0 = error "non-positive shares"
takeFromCostBasis c n | n > shares c = error "removing nonexisting shares"
takeFromCostBasis c n = (,) (CostBasis n splitPrice) (CostBasis (shares c - n) newPrice)
  where
    avgPrice = totalPrice c / (shares c)
    splitPrice = n * avgPrice
    newPrice = totalPrice c - splitPrice

splitTrade :: Trade -> Decimal -> (Trade, [Trade])
splitTrade t n
  | amount t < n = (t, [])
  | otherwise = (t {amount = n}, [t {amount = m}])
  where
    m = amount t - n

actionOrder :: Action -> Int
actionOrder Sell = 0
actionOrder _ = 1

groupBySymbol :: [Trade] -> HashMap String [Trade]
groupBySymbol x = fromListWith (++) [(Lib.symbol t, [t]) | t <- x]

maybeParseISODate :: String -> Maybe Day
maybeParseISODate text = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" text

maybeParseAmericanDate :: String -> Maybe Day
maybeParseAmericanDate text =
  parseTimeM True defaultTimeLocale "%-m/%-d/%Y" text

maybeParseDate :: String -> Maybe Day
maybeParseDate s = maybeParseISODate s A.<|> maybeParseAmericanDate s

parseFX :: String -> IO (Either String (V.Vector FXRow))
parseFX name = do
  csvData <- BL.readFile name
  return $ snd <$> decodeByName csvData

parseTrades :: String -> IO (Either String [Trade])
parseTrades name = do
  csvData <- BL.readFile name
  return $ V.toList <$> (snd <$> decodeByName csvData)

usdToGbp :: (V.Vector FXRow) -> HashMap Day Decimal
usdToGbp =
  foldl (\a p -> HM.insert (date p) ((gbp p) / (usd p)) a) HM.empty

mLookup :: HashMap Day b -> Day -> Either String b
mLookup h k =
  case HM.lookup k h of
    Just x -> return x
    Nothing -> A.empty -- fail $ "failed to lookup " ++ (show k)

concatPair :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
concatPair (x1, y1) (x2, y2) = ((x1 ++ x2), (y1 ++ y2))

matchingFutureTrades :: Day -> Decimal -> [Trade] -> ([Trade], [Trade])
matchingFutureTrades _ _ [] = ([], [])
matchingFutureTrades _ 0 ts = ([], ts)
matchingFutureTrades d _ a@(t : ts) | tradeDate t > d = ([], a)
matchingFutureTrades d n (t@Trade {action = Sell} : ts) =
  concatPair ([], [t]) $ matchingFutureTrades d n ts
matchingFutureTrades d n (t@Trade {action = Buy} : ts) =
  concatPair ([x], []) $ matchingFutureTrades d (n - amount x) (res ++ ts)
  where
    (x, res) = splitTrade t n

getCapitalGains :: Trade -> CostBasis -> CapitalGains
getCapitalGains t cb = CapitalGains cb (totalValue t - totalPrice cb) (tradeDate t)

handleTradesSorted :: CostBasis -> [Trade] -> [CapitalGains]
handleTradesSorted cb [] = []
handleTradesSorted cb (t@Trade {action = Buy} : ts) =
  handleTradesSorted (addToCostBasis t cb) ts
handleTradesSorted cb (t@Trade {action = Sell} : ts) =
  cg : handleTradesSorted newCB rest
  where
    maxFuture = addDays 30 $ tradeDate t
    -- Find trades in the next 30 days that can match up with the sale.
    (matching, rest) = matchingFutureTrades maxFuture (amount t) ts
    -- Add them up to form the costbasis for this trade.
    futureCB = Data.List.foldr addToCostBasis (CostBasis 0 0) matching
    -- See if we've fully matched the trade with them.
    missing = (amount t) - (shares futureCB)
    -- If not, take some from the holding.
    (takenCB, newCB) = takeFromCostBasis cb missing
    saleCB = addCostBases futureCB takenCB
    cg = getCapitalGains t saleCB

handleTrades :: [Trade] -> [CapitalGains]
handleTrades t = handleTradesSorted (CostBasis 0 0) sortedT
  where
    -- Sort by date, with sales always coming before buys. This way, we can
    -- implement the same-day and the Bed and Breakfast rule in the same way.
    sortedT = sortOn (tradeDate &&& actionOrder . action) t

multiplyFMV :: Trade -> Decimal -> Trade
multiplyFMV t n = t {fmv = fmv t * n}

getPoundValues ::
  HashMap Day Decimal -> [Trade] -> Either String [Trade]
getPoundValues h t =
  zipWith multiplyFMV t <$> rates
  where
    rates = mapM (mLookup h . tradeDate) t

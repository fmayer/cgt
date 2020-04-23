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
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( parseFX
  , parseTrades
  , amount
  , fmv
  , date
  , getPoundValues
  , gbp
  , usd
  , usdToGbp
  , tradeDate
  , symbol
  , maybeParseAmericanDate
  , maybeParseISODate
  , groupBySymbol
  , actionOrder
  , action
  , takeFromCostBasis
  , addCostBases
  , CostBasis (..)
  , Trade (..)
  , Action (..)
  , CapitalGains (..)
  , handleTrades
  ) where

import qualified Control.Applicative  as A
import           Control.Arrow
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import           Data.Decimal
import           Data.Hashable
import           Data.HashMap.Lazy    as HM
import           Data.List
import           Data.Maybe
import           Data.Text            (Text, unpack)
import           Data.Text.Encoding
import           Data.Time.Calendar
import           Data.Time.Format
import qualified Data.Vector          as V
import           GHC.Generics         (Generic)

data FXRow =
  FXRow
    { date :: !Day
    , gbp  :: !Decimal
    , usd  :: !Decimal
    }
  deriving (Generic, Show)

data Action
  = Buy
  | Sell
  deriving (Show)

data Trade =
  Trade
    { symbol    :: !String
    , action    :: !Action
    , tradeDate :: !Day
    , amount    :: !Int
    , fmv       :: !Decimal
    }
  deriving (Generic, Show)

data CostBasis =
  CostBasis
    { shares     :: !Int
    , totalPrice :: !Decimal
    }
  deriving (Generic, Show, Eq)

data CapitalGains =
  CapitalGains
    { costBasis :: !CostBasis,
      gains     :: !Decimal
    }
  deriving (Generic, Show, Eq)

instance FromNamedRecord FXRow where
  parseNamedRecord r = FXRow <$> r .: "Date" <*> r .: "GBP" <*> r .: "USD"

instance FromNamedRecord Trade where
  parseNamedRecord r =
    Trade <$> r .: "Symbol" <*> r .: "Action" <*> r .: "Date" <*>
    r .: "Quantity" <*> r .: "Vest FMV"

instance Hashable Day where
  hashWithSalt s a = hashWithSalt s $ showGregorian a

instance FromField Action where
  parseField "Deposit" = A.pure Buy
  parseField "Sell"    = A.pure Sell
  parseField _         = A.empty

instance FromField Decimal where
  parseField s =
    case reads . unpack . decodeUtf8 $ s of
      [(x, "")] -> A.pure x
      _         -> A.empty

instance FromField Day where
  parseField s =
    case maybeParseDate . unpack . decodeUtf8 $ s of
      Just d  -> A.pure d
      Nothing -> A.empty

totalValue t = fromIntegral (amount t) * fmv t

addToCostBasis :: Trade -> CostBasis -> CostBasis
addToCostBasis t c = CostBasis (amount t + shares c) (totalValue t + totalPrice c)

addCostBases (CostBasis s1 t1) (CostBasis s2 t2) = CostBasis (s1 + s2) (t1 + t2)

takeFromCostBasis :: CostBasis -> Int -> (CostBasis, CostBasis)
takeFromCostBasis c 0 = ((CostBasis 0 0), c)
takeFromCostBasis c n | n > shares c = error "removing nonexisting shares"
takeFromCostBasis c n = (,) (CostBasis n splitPrice) (CostBasis (shares c - n) newPrice)
  where avgPrice = totalPrice c / (fromIntegral $ shares c)
        splitPrice = fromIntegral n * avgPrice
        newPrice = totalPrice c - splitPrice

splitTrade :: Trade -> Int -> (Trade, [Trade])
splitTrade t n
  | amount t < n = (t, [])
  | otherwise = (t { amount = n}, [t { amount = m}])
  where m = amount t - n

actionOrder :: Action -> Int
actionOrder Sell = 0
actionOrder _    = 1

groupBySymbol :: [Trade] -> HashMap String [Trade]
groupBySymbol x =  fromListWith (++) [(Lib.symbol t, [t]) | t <- x]

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
  return $ fmap snd $ decodeByName csvData

parseTrades :: String -> IO (Either String (V.Vector Trade))
parseTrades name = do
  csvData <- BL.readFile name
  return $ fmap snd $ decodeByName csvData

usdToGbp :: (V.Vector FXRow) -> HashMap Day Decimal
usdToGbp =
  foldl (\a p -> HM.insert (date p) ((gbp p) / (usd p)) a) HM.empty

mLookup :: HashMap Day b -> Day -> Either String b
mLookup h k =
  case HM.lookup k h of
    Just x  -> return x
    Nothing -> fail $ "failed to lookup " ++ (show k)

concatPair :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
concatPair (x1, y1) (x2, y2) = ((x1 ++ x2), (y1 ++ y2))

matchingFutureTrades :: Day -> Int -> [Trade] -> ([Trade], [Trade])
matchingFutureTrades _ _ [] = ([], [])
matchingFutureTrades _ 0 ts = ([], ts)
matchingFutureTrades d _ a@(t:ts) | tradeDate t > d = ([],a)
matchingFutureTrades d n (t@Trade{action=Sell}:ts) =
  concatPair ([], [t]) $ matchingFutureTrades d n ts
matchingFutureTrades d n (t@Trade{action=Buy}:ts) =
  concatPair ([x], []) $ matchingFutureTrades d n (res ++ ts)
  where (x, res) = splitTrade t n

getCapitalGains :: Trade -> CostBasis -> CapitalGains
getCapitalGains t cb = CapitalGains cb (totalValue t - totalPrice cb)

handleTradesSorted :: CostBasis -> [Trade] -> [CapitalGains]
handleTradesSorted cb [] = []
handleTradesSorted cb (t@Trade{action=Buy}:ts) = handleTradesSorted (addToCostBasis t cb) ts
handleTradesSorted cb (t@Trade{action=Sell}:ts) = cg:handleTradesSorted newCB rest
  where maxFuture = addDays 30 $ tradeDate t
        (matching, rest) = matchingFutureTrades maxFuture (amount t) ts
        futureCB = Data.List.foldr addToCostBasis (CostBasis 0 0) matching
        missing = (amount t) - (shares futureCB)
        (takenCB, newCB) = takeFromCostBasis cb missing
        saleCB = addCostBases futureCB  takenCB
        cg = getCapitalGains t saleCB

handleTrades :: [Trade] -> [CapitalGains]
handleTrades t = handleTradesSorted (CostBasis 0 0) sortedT
  where sortedT = sortOn (tradeDate &&& actionOrder . action) t

getPoundValues ::
     HashMap Day Decimal -> V.Vector Trade -> Either String (V.Vector Decimal)
getPoundValues h t =
  V.zipWith (*) (fmap totalValue t) <$> rates
  where
    rates = mapM (mLookup h . tradeDate) t

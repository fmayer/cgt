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
  , getPoundTrades
  , gbp
  , usd
  , usdToGbp
  , tradeDate
  ) where

import qualified Control.Applicative  as A
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import           Data.Decimal
import           Data.Hashable
import           Data.HashMap.Lazy
import           Data.Text            (Text, unpack)
import           Data.Text.Encoding
import           Data.Time.Calendar
import           Data.Time.Format
import qualified Data.Vector          as V
import           GHC.Generics         (Generic)

data Holding = 
  Holding
    { total_amount :: !Int
    , total_value :: !Decimal
    }
  deriving (Generic, Show)


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
    { action    :: !Action
    , tradeDate :: !Day
    , amount    :: !Int
    , fmv       :: !Decimal
    }
  deriving (Generic, Show)


data GBPTrade = GBPTrade Decimal Trade

instance FromNamedRecord FXRow where
  parseNamedRecord r = FXRow <$> r .: "Date" <*> r .: "GBP" <*> r .: "USD"

instance FromNamedRecord Trade where
  parseNamedRecord r =
    Trade <$> r .: "Action" <*> r .: "Date" <*> r .: "Quantity" <*>
    r .: "Vest FMV"

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

addBuy :: Holding -> GBPTrade -> Holding
addBuy h (GBPTrade gbpfmv t) = Holding (total_amount h + amount t) (total_value h + gbpfmv * (fromIntegral $ amount t))

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
  case decodeByName csvData of
    Left err     -> return $ Left err
    Right (_, v) -> return $ Right v

parseTrades :: String -> IO (Either String (V.Vector Trade))
parseTrades name = do
  csvData <- BL.readFile name
  case decodeByName csvData of
    Left err     -> return $ Left err
    Right (_, v) -> return $ Right v

usdToGbp :: (V.Vector FXRow) -> HashMap Day Decimal
usdToGbp =
  foldl (\a p -> insert (date p) ((gbp p) / (usd p)) a) Data.HashMap.Lazy.empty

mLookup :: HashMap Day b -> Day -> Either String b
mLookup h k =
  case Data.HashMap.Lazy.lookup k h of
    Just x  -> return x
    Nothing -> fail $ "failed to lookup " ++ (show k)

getPoundTrades ::
     HashMap Day Decimal -> V.Vector Trade -> Either String (V.Vector GBPTrade)
getPoundTrades h t = V.zipWith GBPTrade <$> pounds <*> pure t
  where
    rates = mapM (mLookup h . tradeDate) t
    pounds = V.zipWith (*) (fmap fmv t) <$> rates

-- handleTrade :: (Holding, [GBPTrade]) -> GBPTrade -> (Holding, [GBPTrade])
-- handleTrade (h, []) x@(GBPTrade gbpfmv t) = case action t of
--   Buy -> ((addBuy h x), ts)
--   Sell -> (h, [x])

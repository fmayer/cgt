{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleInstances #-}

module Lib
    (  parseFX
      , parseTrades
      , date
      , gbp
      , usd
      , usdToGbp
    ) where

import Control.Applicative
import Control.Monad
import Data.Csv
import Data.Decimal
import Data.Text    (Text, unpack)
import Data.Text.Encoding
import Data.Time.Calendar
import Data.Time.Format
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.HashMap.Lazy
import Data.Hashable

data FXRow = FXRow { date :: !Day , gbp :: !Decimal, usd :: !Decimal }
    deriving (Generic, Show)

data Action = Buy | Sell deriving (Show)
data Trade = Trade { action :: !Action, tradeDate :: !Day,  amount :: !Int,  fmv :: !Decimal } deriving (Generic, Show)

instance FromNamedRecord FXRow where
    parseNamedRecord r = FXRow <$> r .: "Date" <*> r .: "GBP" <*> r .: "USD"

instance FromNamedRecord Trade where
    parseNamedRecord r = Trade <$> r .: "Action" <*> r .: "Date" <*> r .: "Quantity" <*> r .: "Vest FMV"

instance Hashable Day where
  hashWithSalt s a = hashWithSalt s $ showGregorian a

maybeParseISODate :: String -> Maybe Day
maybeParseISODate text = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" text

maybeParseAmericanDate :: String -> Maybe Day
maybeParseAmericanDate text = parseTimeM True defaultTimeLocale "%-m/%-d/%Y" text

maybeParseDate :: String -> Maybe Day
maybeParseDate s = case maybeParseISODate s of
    Just d -> Just d
    Nothing -> maybeParseAmericanDate s

instance FromField Action where
  parseField "Deposit" = pure Buy
  parseField "Sell" = pure Sell
  parseField _ = Control.Applicative.empty

instance FromField Decimal where
  parseField s = case reads $ unpack $ decodeUtf8 s of
              [(x, "")] -> pure x
              _ -> Control.Applicative.empty

instance FromField Day where
  parseField s = case (maybeParseDate $ unpack $ decodeUtf8 s) of
    Just d -> pure d
    Nothing -> Control.Applicative.empty

parseFX :: [Char] -> IO (Either String (V.Vector FXRow))
parseFX name = do
    csvData <- BL.readFile name
    case decodeByName csvData of
      Left err -> return $ Left err
      Right (_, v) -> return $ Right v


parseTrades :: [Char] -> IO (Either String (V.Vector Trade))
parseTrades name = do
    csvData <- BL.readFile name
    case decodeByName csvData of
      Left err -> return $ Left err
      Right (_, v) -> return $ Right v

usdToGbp :: (V.Vector FXRow) -> HashMap Day Decimal
usdToGbp = foldl  (\a p -> insert (date p) ((gbp p) / (usd p)) a) Data.HashMap.Lazy.empty

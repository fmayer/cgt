{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}


import           Data.Decimal
import           Data.Time.Calendar
import           Lib
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

instance Arbitrary CostBasis where
  arbitrary = do
    Positive shares <- arbitrary
    Positive price <- arbitrary
    return $ CostBasis shares price

instance Arbitrary Decimal where
  arbitrary = fromRational <$> arbitrary

case_invalidISOMonth = Nothing @=? (maybeParseISODate "2020-20-20")
case_invalidAmericanMonth = Nothing @=? (maybeParseAmericanDate "20/20/2020")

sampleEarlierBuy = Trade "ABC" Buy (fromGregorian 2020 01 01) 1 (fromIntegral 500)
sampleBuy = Trade "ABC" Buy (fromGregorian 2020 01 02) 1 (fromIntegral 1000)

case_onlyBuy = [] @=? handleTrades [sampleBuy]
case_sameDayFull = [(CapitalGains (CostBasis 1 1000) 0 (tradeDate sampleBuy))] @=?
  handleTrades [sampleBuy {action = Sell}, sampleBuy]
case_sameDayPartial = [(CapitalGains (CostBasis 2 1500) 500 (tradeDate sampleBuy))] @=?
  handleTrades [sampleEarlierBuy, sampleBuy {action = Sell, amount = 2}, sampleBuy]
case_sameDayPartialFraction = [(CapitalGains (CostBasis 2 1750) 250 (tradeDate sampleBuy))] @=?
  handleTrades [sampleEarlierBuy, sampleBuy {action = Sell, amount = 2}, sampleBuy { amount = 1.5 }]
case_empty = handleTrades [] @=? []

case_taxYearApr5 = 2019 @=? getTaxYear (fromGregorian 2020 4 5)
case_taxYearApr6 = 2020 @=? getTaxYear (fromGregorian 2020 4 6)

prop_takeFromCostBasis cb n
  | n <= shares cb = addCostBases cb1 cb2 == cb
  | otherwise = True
  where (cb1, cb2) = takeFromCostBasis cb n

main :: IO ()
main = $(defaultMainGenerator)

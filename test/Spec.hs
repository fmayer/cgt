{-# LANGUAGE TemplateHaskell #-}

import           Lib
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit

case_invalidISOMonth = Nothing @=? (maybeParseISODate "2020-20-20")
case_invalidAmericanMonth = Nothing @=? (maybeParseAmericanDate "20/20/2020")

main :: IO ()
main = $(defaultMainGenerator)

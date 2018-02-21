{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Preamble
import qualified Test.Labsat.Parser as Parser
import           Test.Tasty

tests :: TestTree
tests =
  testGroup "Tests"
    [ Parser.tests
    ]

main :: IO ()
main = defaultMain tests

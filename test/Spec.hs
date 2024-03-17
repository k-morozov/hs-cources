module Main (main) where
import Test.HUnit
import TestDataType
import qualified System.Exit as Exit


main :: IO ()
main = do
    result <- runTestTT testDataType
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
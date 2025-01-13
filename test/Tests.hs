module Main where
import Test.HUnit
import ParserTests (parserTests)
import EmitterTests (emitterTests)

tests :: Test
tests = TestList [ parserTests, emitterTests ]

main :: IO ()
main = runTestTTAndExit tests
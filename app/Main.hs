module Main where

import qualified RPNCalc
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "Insert your equation in Reverse Polish Notation Below:"
  putStr "$: "
  hFlush stdout
  expression <- getLine
  case expression of
    "exit" -> return ()
    _ -> do
      newLine
      putStrLn $ calculateResult expression
      newLine
      main

newLine :: IO ()
newLine = putStrLn ""

calculateResult :: String -> String
calculateResult expr = case RPNCalc.calculate expr of
  RPNCalc.Ok value -> "-> " ++ show value
  RPNCalc.Err errString -> "ERROR: " ++ errString


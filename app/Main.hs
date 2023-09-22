{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib

main :: IO ()
main = testFn


{--
  let r = extractBankStmts 9598554 [9598554, 49366127] [9566395, 32159, 49366127] :: [Int]
  putStrLn "input: 9598554 [9598554, 49366127] [9566395, 32159, 49366127]"
  putStr "found: 9598554 == "
  print r
--}

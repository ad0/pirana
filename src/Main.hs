module Main where

import System.Exit

import Parser
import Lts
import Normalization

main :: IO ()
main = do
  file   <- readFile "examples/philo2.pi"
  piFile <- case parsePiProcess file of
    Left  err -> do { putStrLn err ; exitFailure }
    Right dat -> return dat
  let (defs, proc) = intify piFile
  let lts = generateLts defs proc
  dotOutput "examples/philo2.pi.dot" lts
  autOutput "examples/philo2.pi.aut" lts
  putStr   $ show (ltsNbStates lts) ++ " states, "
  putStrLn $ show (ltsNbTransitions lts) ++ " transitions"

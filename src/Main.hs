module Main where

import Names
import Syntax

main :: IO ()
main = do
  let defs = [Def "P" [S "x"] $ Nu (S "a") (OutPref (S "x") (S "a") (Call "P" [S "a"]))]

  let t1 = InPref (S "x") (S "y") Inert
  let t2 = Sum (InPref (S "x") (S "y") Inert)
               (Nu (S "w") (OutPref (S "w") (S "z") Inert))
  let t3 = Par (InPref (S "a") (S "x") Inert)
               (OutPref (S "a") (S "y") Inert)
  let t4 = Par (Sum (InPref (S "a") (S "x") Inert)
                    (InPref (S "c") (S "y") Inert))
               (OutPref (S "c") (S "v") Inert)
  let t5 = Call "P" [S "x"]
  let t6 = InPref (S "a") (S "x") $ InPref (S "a") (S "x") $ InPref (S "a") (S "x") Inert
  let t7 = InPref (S "a") (S "b") $ InPref (S "b") (S "c") $ InPref (S "c") (S "d") Inert

  print t4
  print $ unfold defs t4

module Lts where

import Data.Array
import Data.Array.ST
import Control.Monad.ST

import Syntax
import Hash

type StateID = Int

data LTSState = LTSState { stateUid    :: StateID
                         , statePacked :: PackedProcess
                         , stateNexts  :: [StateID]
                         }

data LTS = LTS { ltsNbStates      :: Int
               , ltsNbTransitions :: Int
               , ltsData          :: Array StateID LTSState
               }

generateLts :: [Def Int] -> Process Int -> LTS
generateLts defs proc = LTS (-1) (-1) $ runSTArray gen
  where gen :: ST s (STArray s Int LTSState)
        gen = newArray (0, 10) $ LTSState 1 0
--        gen = error "generateLTS"

dotOutput :: String -> LTS -> IO ()
dotOutput fname lts = do
  putStrLn "digraph LTS {"
  putStrLn "TODO"
  putStrLn "}"

autOutput :: String -> LTS -> IO ()
autOutput = error "autOutput TODO"

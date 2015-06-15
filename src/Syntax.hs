module Syntax where

import Data.List (intercalate)
import qualified Data.Set as S

data Process a = Inert
               | Nu a (Process a)
               | Local a (Process a)
               | TauPref (Process a)
               | InPref a a (Process a)
               | OutPref a a (Process a)
               | Par (Process a) (Process a)
               | Sum (Process a) (Process a)
               | Call a [a]
               deriving Eq

data Def a = Def { defName   :: a
                 , defParams :: [a]
                 , defBody   :: Process a
                 }

instance Show a => Show (Process a) where
  show Inert           = "0"
  show (Nu n p)        = "new (" ++ show n ++ ")." ++ show p
  show (Local n p)     = "local (" ++ show n ++ ")." ++ show p
  show (TauPref p)     = "tau." ++ show p
  show (InPref a x p)  = show a ++ "?(" ++ show x ++ ")." ++ show p
  show (OutPref a b p) = show a ++ "!" ++ show b ++ "." ++ show p
  show (Par p q)       = "(" ++ show p ++ " | " ++ show q ++ ")"
  show (Sum p q)       = "(" ++ show p ++ " + " ++ show q ++ ")"
  show (Call d ns)     = show d ++ "[" ++ intercalate "," (map show ns) ++ "]"

instance Show a => Show (Def a) where
  show (Def name params body) = "def " ++ show name ++
    "(" ++ intercalate "," (map show params) ++ ") =\n" ++
    show body

freeNames :: (Ord a) => Process a -> S.Set a
freeNames Inert           = S.empty
freeNames (Nu n p)        = S.delete n $ freeNames p
freeNames (Local n p)     = S.delete n $ freeNames p
freeNames (TauPref p)     = freeNames p
freeNames (InPref a x p)  = S.insert a $ S.delete x $ freeNames p
freeNames (OutPref a b p) = S.insert a $ S.insert b $ freeNames p
freeNames (Par p q)       = S.union (freeNames p) (freeNames q)
freeNames (Sum p q)       = S.union (freeNames p) (freeNames q)
freeNames (Call _ ns)     = S.fromList ns


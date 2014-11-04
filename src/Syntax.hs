module Syntax where

import Names

import Data.List (intercalate, find)
import qualified Data.Set as S

data Process = Inert
             | Nu Name Process
             | TauPref Process
             | InPref Name Name Process
             | OutPref Name Name Process
             | Par Process Process
             | Sum Process Process
             | Call String [Name]
             deriving Eq

data Def = Def { defName   :: String
               , defParams :: [Name]
               , defBody   :: Process
               }

instance Names Process where
  freeNames Inert           = S.empty
  freeNames (Nu _ p)        = freeNames p
  freeNames (TauPref p)     = freeNames p
  freeNames (InPref a x p)  = S.insert a $ S.delete x $ freeNames p
  freeNames (OutPref a b p) = S.insert a $ S.insert b $ freeNames p
  freeNames (Par p q)       = S.union (freeNames p) (freeNames q)
  freeNames (Sum p q)       = S.union (freeNames p) (freeNames q)
  freeNames (Call _ ns)     = S.fromList ns

  boundNames Inert           = S.empty
  boundNames (Nu n p)        = S.insert n $ boundNames p
  boundNames (TauPref p)     = boundNames p
  boundNames (InPref _ x p)  = S.insert x $ boundNames p
  boundNames (OutPref _ _ p) = boundNames p
  boundNames (Par p q)       = S.union (boundNames p) (boundNames q)
  boundNames (Sum p q)       = S.union (boundNames p) (boundNames q)
  boundNames (Call _ _)      = S.empty

instance Show Process where
  show Inert           = "0"
  show (Nu n p)        = "(" ++ show n ++ ")" ++ show p
  show (TauPref p)
    | p == Inert       = "tau"
    | otherwise        = "tau." ++ show p
  show (InPref a x p)
    | p == Inert       = show a ++ "?(" ++ show x ++ ")"
    | otherwise        = show a ++ "?(" ++ show x ++ ")." ++ show p
  show (OutPref a b p)
    | p == Inert       = show a ++ "!" ++ show b
    | otherwise        = show a ++ "!" ++ show b ++ "." ++ show p
  show (Par p q)       = "(" ++ show p ++ " | " ++ show q ++ ")"
  show (Sum p q)       = "(" ++ show p ++ " + " ++ show q ++ ")"
  show (Call d ns)     = d ++ "[" ++ intercalate "," (map show ns) ++ "]"

unfold :: [Def] -> Process -> Process
unfold _ Inert           = Inert
unfold defs (Nu n p)        = Nu n (unfold defs p)
unfold _ (TauPref p)     = TauPref p
unfold _ (InPref a x p)  = InPref a x p
unfold _ (OutPref a b p) = OutPref a b p
unfold defs (Par p q)    = Par (unfold defs p) (unfold defs q)
unfold defs (Sum p q)    = Sum (unfold defs p) (unfold defs q)
unfold defs (Call d ns)  = case find ((== d) . defName) defs of
  Just (Def _ args p) -> error "TODO"
  Nothing             -> error $ "unknown process def: " ++ d


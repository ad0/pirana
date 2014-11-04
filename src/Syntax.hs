module Syntax where

import Names

import qualified Data.Set as S

data Process = Inert
             | Nu Name Process
             | TauPref Process
             | InPref Name Name Process
             | OutPref Name Name Process
             | Par Process Process
             | Sum Process Process
             | Call Def [Name]

data Def = Def String [Name] Process

instance Names Process where
  freeNames Inert           = S.empty
  freeNames (Nu n p)        = freeNames p
  freeNames (TauPref p)     = freeNames p
  freeNames (InPref a x p)  = S.insert a $ S.delete x $ freeNames p
  freeNames (OutPref a b p) = S.insert a $ S.insert b $ freeNames p
  freeNames (Par p q)       = S.union (freeNames p) (freeNames q)
  freeNames (Sum p q)       = S.union (freeNames p) (freeNames q)
  freeNames (Call d ns)     = S.fromList ns

  boundNames Inert           = S.empty
  boundNames (Nu n p)        = S.insert n $ boundNames p
  boundNames (TauPref p)     = boundNames p
  boundNames (InPref a x p)  = S.insert x $ boundNames p
  boundNames (OutPref a b p) = boundNames p
  boundNames (Par p q)       = S.union (boundNames p) (boundNames q)
  boundNames (Sum p q)       = S.union (boundNames p) (boundNames q)
  boundNames (Call d ns)     = S.empty

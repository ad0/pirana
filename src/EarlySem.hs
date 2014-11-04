module EarlySem where

import Syntax

import qualified Data.Set as S

data Action = Tau
            | Input Name Name
            | Output Name Name
            | BoundOutput Name Name

instance Names Action where
  freeNames Tau               = S.empty
  freeNames (Input a x)       = S.singleton a
  freeNames (Output a b)      = S.insert a $ S.singleton b
  freeNames (BoundOutput a x) = S.singleton a

  boundNames Tau               = S.empty
  boundNames (Input a x)       = S.singleton x
  boundNames (Output a b)      = S.empty
  boundNames (BoundOutput a x) = S.singleton x

derivatives :: Process -> [(Action,Process)]
derivatives = error "TODO"

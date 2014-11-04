module Names where

import qualified Data.Set as S

data Name = S String | I Int
  deriving (Eq,Ord)

instance Show Name where
  show (S s) = s
  show (I i) = show i

class Names a where
  freeNames  :: a -> S.Set Name
  boundNames :: a -> S.Set Name
  names      :: a -> S.Set Name
  names x = S.union (freeNames x) (boundNames x)
  

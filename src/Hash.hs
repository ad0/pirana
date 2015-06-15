module Hash (PackedProcess, pack, unpack) where

import Data.Bits
import Control.Monad.State

import Syntax

type PackedProcess = Integer

-- PackState = (accumulator, currentOffset)
type PackState = (PackedProcess, Int)

intSz :: Int
intSz = 6

putBits :: Integer -> Int -> State PackState ()
putBits val size = do
  (h, offset) <- get
  let val' = val `shift` offset
  put (h .|. val', offset + size)

putTag :: Integer -> State PackState ()
putTag tag = putBits tag 3

putBit :: Integer -> State PackState ()
putBit tag = putBits tag 1

putInt :: Int -> State PackState ()
putInt val = putBits (toInteger val) intSz

getBits :: Int -> Int -> State Integer Int
getBits sz mask = do
  h <- liftM fromInteger get
  let val = h .&. mask
  put $ toInteger $ h `shiftR` sz
  return val

getTag :: State Integer Int
getTag = getBits 3 7

getBit :: State Integer Int
getBit = getBits 1 1

getInt :: State Integer Int
getInt = getBits intSz 63


pack :: Process Int -> Integer
pack proc = fst $ execState (pack' proc) (0,0)

pack' :: Process Int -> State PackState ()
pack' Inert           = putTag 0
pack' (Nu n p)        = do { putTag 1; putBit 0; putInt n; pack' p }
pack' (Local n p)     = do { putTag 1; putBit 1; putInt n; pack' p }
pack' (TauPref p)     = do { putTag 2; pack' p }
pack' (InPref c d p)  = do { putTag 3; putInt c; putInt d; pack' p }
pack' (OutPref c x p) = do { putTag 4; putInt c; putInt x; pack' p }
pack' (Par p q)       = do { putTag 5; pack' p; pack' q }
pack' (Sum p q)       = do { putTag 6; pack' p; pack' q }
pack' (Call n args)   = do
  putTag 7
  putInt n
  putInt $ length args
  forM_ args putInt



unpack :: Integer -> Process Int
unpack = evalState unpack'

unpack' :: State Integer (Process Int)
unpack' = do
  tag <- getTag
  case tag of
    0 -> return Inert
    1 -> do
           t <- getBit
           let f = case t of { 0 -> Nu ; 1 -> Local ; _ -> error "unpack" }
           n <- getInt
           p <- unpack'
           return $ f n p
    2 -> liftM TauPref unpack'
    3 -> liftM3 InPref getInt getInt unpack'
    4 -> liftM3 OutPref getInt getInt unpack'
    5 -> liftM2 Par unpack' unpack'
    6 -> liftM2 Sum unpack' unpack'
    7 -> liftM2 Call getInt (getInt >>= flip replicateM getInt)
    _ -> error "unpack process: invalid tag"


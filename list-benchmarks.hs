import Criterion.Main   ( defaultMain, bgroup, bench, whnf, Pure )

import GHC.Conc         ( numCapabilities )

import Control.Parallel ( par, pseq )

import Data.Monoid      ( Monoid(..), Sum(..) )
import Data.Foldable    ( foldMap, foldl' )

import Data.Sequence    ( Seq )
import qualified Data.Sequence as Seq

import Data.Vector.Unboxed ( Vector, Unbox )
import qualified Data.Vector.Unboxed as Vec

main :: IO ()
main = defaultMain [
         bgroup "seq" [
           bench "List"    $ whnf sum [1..len],
           bench "Seq"     $ whnf (foldl' (+) 0) (Seq.fromList [1..len]),
           bench "UVector" $ whnf (Vec.foldl' (+) 0) (Vec.fromList [1..len])],
         bgroup "par" [
           bench "List"    $ sumWith foldList [1..len],
           bench "Seq"     $ sumWith foldSeq  [1..len],
           bench "UVector" $ sumWith foldVec  [1..len]]]
  where len = 100000

type ListFold m a = (a -> m) -> [a] -> m

sumWith :: ListFold (Sum Int) Int -> [Int] -> Pure
sumWith fold = whnf (getSum . fold Sum)

-- ordinary lists
foldList :: Monoid m => ListFold m a
{-# SPECIALIZE foldList :: ListFold (Sum Int) Int #-}
foldList = foldListWithSparks numCapabilities mappend mempty

-- with upper limit for sparks
foldListWithSparks :: Int -> (m -> m -> m) -> m -> (a -> m) -> [a] -> m
foldListWithSparks cnt append empty f = worker cnt
  where
    worker _ []               = empty
    worker _ [x]              = f $! x
    worker n xsys | n <= 1    = foldl' (flip (append . f)) empty xsys
                  | otherwise = x `par` y `pseq` append x y
      where
        (xs,ys) = splitAt (length xsys `div` 2) xsys
        m       = n `div` 2
        x       = worker m xs
        y       = worker (n-m) ys

-- finger trees
foldSeq :: Monoid m => ListFold m a
{-# SPECIALIZE foldSeq :: ListFold (Sum Int) Int #-}
foldSeq f = foldSeqWithSparks numCapabilities mappend mempty f . Seq.fromList

foldSeqWithSparks :: Int -> (m -> m -> m) -> m -> (a -> m) -> Seq a -> m
foldSeqWithSparks cnt append empty f sq = worker cnt sq
  where
    worker n s | n <= 1    = foldl' (flip (append . f)) empty s
               | otherwise = case Seq.length s of
                               0 -> empty
                               1 -> f $! Seq.index s 0
                               k -> let l       = k `div` 2
                                        (xs,ys) = Seq.splitAt l s
                                        m       = n `div` 2
                                        x       = worker m xs
                                        y       = worker (n-m) ys
                                     in x `par` y `pseq` append x y

-- unboxed vectors
foldVec :: (Monoid m, Unbox a) => ListFold m a
foldVec f = foldVecWithSparks numCapabilities mappend mempty f . Vec.fromList

foldVecWithSparks :: Unbox a
                  => Int -> (m -> m -> m) -> m -> (a -> m) -> Vector a -> m
foldVecWithSparks cnt append empty f vec = worker cnt vec
  where
    worker n v | n <= 1    = Vec.foldl' (flip (append . f)) empty v
               | otherwise = case Vec.length v of
                               0 -> empty
                               1 -> f $! Vec.head v
                               k -> let l       = k `div` 2
                                        (xs,ys) = Vec.splitAt l v
                                        m       = n `div` 2
                                        x       = worker m xs
                                        y       = worker (n-m) ys
                                     in x `par` y `pseq` append x y
import Control.Parallel ( par, pseq )

import Data.Monoid      ( Monoid(..), Sum(..) )
import Data.Foldable    ( foldMap )

import Data.List        ( foldl' )

import Data.Sequence    ( Seq )
import qualified Data.Sequence as Seq

import GHC.Conc         ( numCapabilities )

import Criterion.Main   ( defaultMain, bgroup, bench, whnf, Pure )

main :: IO ()
main = defaultMain [
         bgroup "sequential" [
           bench "ordinary lists" $ whnf sum [1..len],
           bench "finger trees"   $ whnf (getSum . foldMap Sum)
                                         (Seq.fromList [1..len])],
         bgroup "parallel" [
           bgroup "top down" [
             bench "ordinary lists" $ sumWith foldList [1..len],
             bench "finger trees"   $ sumWith foldSeq  [1..len]]]]
  where len = 10^5

type ListFold m a = (a -> m) -> [a] -> m

sumWith :: ListFold (Sum Integer) Integer -> [Integer] -> Pure
sumWith fold = whnf (getSum . fold Sum)

-- ordinary lists
foldList :: Monoid m => ListFold m a
{-# SPECIALIZE foldList :: ListFold (Sum Integer) Integer #-}
foldList = foldListWithSparks numCapabilities mappend mempty

-- with upper limit for sparks
foldListWithSparks :: Int -> (m -> m -> m) -> m -> (a -> m) -> [a] -> m
foldListWithSparks cnt append empty f = worker cnt
  where
    worker _ []               = empty
    worker _ [x]              = f x
    worker n xsys | n <= 1    = foldl' append empty $ map f xsys
                  | otherwise = x `par` y `pseq` append x y
      where
        (xs,ys) = splitAt (length xsys `div` 2) xsys
        m       = n `div` 2
        x       = worker m xs
        y       = worker (n-m) ys

-- finger trees
foldSeq :: Monoid m => ListFold m a
{-# SPECIALIZE foldSeq :: ListFold (Sum Integer) Integer #-}
foldSeq f = foldSeqWithSparks numCapabilities mappend mempty f . Seq.fromList

foldSeqWithSparks :: Int -> (m -> m -> m) -> m -> (a -> m) -> Seq a -> m
foldSeqWithSparks cnt append empty f sq = worker cnt sq
  where
    worker n s = case Seq.length s of
                   0 -> empty
                   1 -> f (Seq.index s 0)
                   k -> let l       = k `div` 2
                            (xs,ys) = Seq.splitAt l s
                            m       = n `div` 2
                            x       = worker m xs
                            y       = worker (n-m) ys
                         in x `par` y `pseq` append x y

import Criterion.Main   ( defaultMain, bench, whnf, Pure )

import GHC.Conc         ( numCapabilities )

import Control.Parallel ( par, pseq )

import Data.Monoid      ( Monoid(..), Sum(..) )
import Data.Foldable    ( foldl' )

import Data.Vector.Unboxed ( Vector, Unbox )
import qualified Data.Vector.Unboxed as Vec
import System.Random.Mersenne ( getStdRandom, randoms )

main :: IO ()
main = do let len = 10000000
          list <- fmap (take len . map (`rem`100)) $ getStdRandom randoms
          let vec = Vec.fromList (list :: [Int])
          defaultMain [
            bench "List" $ sumWith foldList list,
            bench "Vec"  $ sumWith foldVec vec]

sumWith :: ((Int -> Sum Int) -> a -> Sum Int) -> a -> Pure
sumWith fold = whnf (getSum . fold Sum)

-- ordinary lists
foldList :: Monoid m => (a -> m) -> [a] -> m
{-# SPECIALIZE foldList :: (Int -> Sum Int) -> [Int] -> Sum Int #-}
foldList = foldListWithSparks numCapabilities mappend mempty

-- with upper limit for sparks
foldListWithSparks :: Int -> (m -> m -> m) -> m -> (a -> m) -> [a] -> m
foldListWithSparks cnt append empty f = worker cnt
  where
    worker _ []               = empty
    worker _ [x]              = f $! x
    worker n xsys | n <= 1    = foldl' (flip (append . f)) empty xsys
                  | otherwise = y `par` x `pseq` append x y
      where
        (xs,ys) = splitAt (length xsys `div` 2) xsys
        m       = n `div` 2
        x       = worker m xs
        y       = worker (n-m) ys

-- unboxed vectors
foldVec :: (Monoid m, Unbox a) => (a -> m) -> Vector a -> m
{-# SPECIALIZE foldVec :: (Int -> Sum Int) -> Vector Int -> Sum Int #-}
foldVec f = foldVecWithSparks numCapabilities mappend mempty f

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
                                     in y `par` x `pseq` append x y

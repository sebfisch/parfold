import GHC.Conc         ( numCapabilities )

import Control.Parallel ( par, pseq )

import Data.Monoid      ( Monoid(..), Sum(..) )

import Data.Vector.Unboxed ( Vector, Unbox )
import qualified Data.Vector.Unboxed as Vec

import System.Environment

main :: IO ()
main = do len <- fmap (read.head) getArgs
          print . getSum . foldVec Sum $ Vec.generate len succ

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
                                     in x `par` y `pseq` append x y

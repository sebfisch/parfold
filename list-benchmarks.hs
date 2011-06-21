import Control.Parallel ( par, pseq )

import Data.Monoid      ( Monoid(..), Sum(..) )
import Data.List        ( foldl' )

import Data.Sequence    ( Seq )
import qualified Data.Sequence as Seq

import GHC.Conc         ( numCapabilities )

import Criterion.Main   ( defaultMain, bgroup, bench, whnf, Pure )

main :: IO ()
main = defaultMain [
         bgroup "top down" [
           bench "ordinary lists" $ sumWith foldList [1..len],
           bench "finger trees"   $ sumWith foldSeq  [1..len]]]
  where len = 10^5

type ListFold m a = (a -> m) -> [a] -> m

sumWith :: ListFold (Sum Integer) Integer -> [Integer] -> Pure
sumWith fold = whnf (getSum . fold Sum)

-- ordinary lists
foldList :: Monoid m => ListFold m a
foldList = foldListWithSparks numCapabilities

-- with upper limit for sparks
foldListWithSparks :: Monoid m => Int -> ListFold m a
foldListWithSparks cnt f = worker cnt
  where
    worker _ []               = mempty
    worker _ [x]              = f x
    worker n xsys | n <= 1    = foldl' mappend mempty $ map f xsys
                  | otherwise = x `par` y `pseq` mappend x y
      where
        (xs,ys) = splitAt (length xsys `div` 2) xsys
        m       = n `div` 2
        x       = worker m xs
        y       = worker (n-m) ys

-- finger trees
foldSeq :: Monoid m => ListFold m a
foldSeq f = foldSeqWithSparks numCapabilities f . Seq.fromList

foldSeqWithSparks :: Monoid m => Int -> (a -> m) -> Seq a -> m
foldSeqWithSparks cnt f sq = worker cnt sq
  where
    worker n s = case Seq.length s of
                   0 -> mempty
                   1 -> f (Seq.index s 0)
                   k -> let l       = k `div` 2
                            (xs,ys) = Seq.splitAt l s
                            m       = n `div` 2
                            x       = worker m xs
                            y       = worker (n-m) ys
                         in x `par` y `pseq` mappend x y

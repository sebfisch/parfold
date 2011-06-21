import Control.Parallel ( par, pseq )

import Data.Monoid      ( Monoid(..), Sum(..) )
import Data.List        ( foldl' )

import GHC.Conc         ( numCapabilities )

import Criterion.Main   ( defaultMain, bgroup, bench, whnf, Pure )

main :: IO ()
main = defaultMain [
         bgroup "top-down" [
           bench "listFold" $ sumWith foldList [1..len]]]
  where len = 10^5

type ListFold m a = (a -> m) -> [a] -> m

sumWith :: ListFold (Sum Integer) Integer -> [Integer] -> Pure
sumWith fold = whnf (getSum . fold Sum)

-- ordinary lists
foldList :: Monoid m => ListFold m a
foldList = foldListWithSparks numCapabilities

-- with upper limit for sparks
foldListWithSparks :: Monoid m => Int -> ListFold m a
foldListWithSparks _ _ []               = mempty
foldListWithSparks _ f [x]              = f x
foldListWithSparks n f xsys | n <= 1    = foldl' mappend mempty $ map f xsys
                            | otherwise = x `par` y `pseq` mappend x y
  where (xs,ys) = splitAt (length xsys `div` 2) xsys
        m       = n `div` 2
        x       = foldListWithSparks m f xs
        y       = foldListWithSparks (n-m) f ys

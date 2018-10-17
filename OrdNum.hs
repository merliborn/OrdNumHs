-- OrdNumPR
-- 順序数の記述および算術計算を行う
-- 
-- OrdNumPRで取り扱える順序数はω^ωより小さいものとする。

module OrdNum where

class OrdNum a where
    normalize          :: a -> a
    normalizeWithLevel :: Natural -> a -> a
    
    normalize = normalizeWithLevel 0
    
    (:+), (:*), (:^) :: a -> a -> a
    
    toOrdNum :: Natural -> a

module OrdNum.OrdNumPR where
import Numeric.Natural

data OrdShort = Zero | L Natural OrdShort
  deriving(Eq, Read, Show)

instance OrdNum OrdShort where
    normalizeWithLevel n Zero    = Zero
    normalizeWithLevel n (L k a) = if n> k then normalizeWithLevel n a
                                           else L k (normalizeWithLevel k a)
    
    toOrdNum 0 = Zero
    toOrdNum n = L 0 (toOrdNum pred n)
    

indexOf :: OrdShort -> Natural
indexOf Zero    = 0
indexOf (L k a) = k

instance Ord OrdShort where
    Zero    <= b       = True
    (L m a) <= Zero    = False
    (L m a) <= (L n b) = case compare m n of { LT -> True;
                                               GT -> False;
                                               EQ -> (normalize a) <= (normalize b) }



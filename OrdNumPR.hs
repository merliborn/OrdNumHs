module OrdNum.OrdNumPR where
import Numeric.Natural
import OrdNum

data OrdShort = Zero | L Natural OrdShort
  deriving(Eq, Read, Show)

instance OrdNum OrdShort where
    normalizeWithLevel n Zero    = Zero
    normalizeWithLevel n (L k a) = if n> k then normalizeWithLevel n a
                                           else L k (normalizeWithLevel k a)
    
    toOrdNum 0 = Zero
    toOrdNum n = L 0 (toOrdNum (pred n))
    
    n +. Zero    = n
    n +. (L k a) = L k (n +. a)
    
    (*.) = undefined
    (^.) = undefined

indexOf :: OrdShort -> Natural
indexOf Zero    = 0
indexOf (L k a) = k

instance Ord OrdShort where
    Zero    <= b       = True
    (L m a) <= Zero    = False
    (L m a) <= (L n b) = case compare m n of { LT -> True;
                                               GT -> False;
                                               EQ -> (normalize a) <= (normalize b) }



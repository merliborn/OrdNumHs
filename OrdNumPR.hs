-- OrdNum.OrdNumPR
-- 
-- type for ordinal numbers <ω^ω
-- 
-- OrdShort
-- Zero  ~= 0
-- L n α ~= α+ω^n

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
    
    a *. Zero    = Zero
    a *. (L k b) = L (k+orderOf a) (a *. b)

    (^.) = undefined

-- indexOf α = max{ n | α=ω^n*α' for some α'}
-- orderOf α = max{ n | ω^n <= α }
indexOf :: OrdShort -> Natural
indexOf a = f (normalize a)
    where f Zero    = 0
          f (L k a) = k

orderOf :: OrdShort -> Natural
orderOf Zero    = 0
orderOf (L k a) = if k>(orderOf a) then k
                                   else orderOf a

instance Ord OrdShort where
    Zero    <= b       = True
    (L m a) <= Zero    = False
    (L m a) <= (L n b) = case compare m n of { LT -> True;
                                               GT -> False;
                                               EQ -> (normalize a) <= (normalize b) }



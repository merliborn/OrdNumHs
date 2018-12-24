module OrdNum where
import Numeric.Natural

class OrdNum a where
    normalize          :: a -> a
    normalizeWithLevel :: Natural -> a -> a
    
    normalize = normalizeWithLevel 0
    
    (+.), (*.), (^.) :: a -> a -> a
    (+:), (*:), (^:) :: a -> a -> a
    
    n +: m = normalize (n +. m)
    n *: m = normalize (n *. m)
    n ^: m = normalize (n ^. m)
    
    toOrdNum :: Natural -> a


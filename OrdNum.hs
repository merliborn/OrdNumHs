-- OrdNumPR
-- 順序数の記述および算術計算を行う
-- 
-- OrdNumPRで取り扱える順序数はω^ωより小さいものとする。

module OrdNum where
import Numeric.Natural

class OrdNum a where
    normalize          :: a -> a
    normalizeWithLevel :: Natural -> a -> a
    
    normalize = normalizeWithLevel 0
    
    (.+), (.*), (.^) :: a -> a -> a
    
    toOrdNum :: Natural -> a


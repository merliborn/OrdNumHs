-- OrdNumPR
-- 順序数の記述および算術計算を行う
-- 
-- OrdNumPRで取り扱える順序数はω^ωより小さいものとする。

module OrdNumPR where


data OrdShort = Ordinal Natural | L Natural OrdShort
  deriving(Eq, Read, Show)

normalize :: OrdShort -> OrdShort
normalize = normalizeWithLevel 0

normalizeWithLevel :: Natural -> OrdShort -> OrdShort
normalizeWithLevel n (Ordinal m) = Ordinal m
normalizeWithLevel n (L k a)     = if n>k then normalizeWithLevel n a
                                          else L k (normalizeWithLevel k a)
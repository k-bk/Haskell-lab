data MyInt = MkMyInt Int
instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2
  (*) (MkMyInt i1) (MkMyInt i2) = i1 * i2


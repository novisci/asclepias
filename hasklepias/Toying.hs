

newtype Pair a b = Pair   { getPair :: (a, b) } deriving (Show)
newtype Period a = Period { getPeriod :: (Pair a a) } deriving (Show)
--newtype PeriodInt = PeriodInt (Period Int) deriving (Show)
type Point    = Period
type Moment   = Period
type Interval = Period

period :: a -> a -> Period a
period a b = Period (Pair (a, b))

--instance Functor (Pair a b) where
--   fmap f (Pair (x, y)) = Pair (f (x, y))

start :: Period a -> a
start x = (fst.getPair.getPeriod) x

end :: Period a -> a
end x = (snd.getPair.getPeriod) x
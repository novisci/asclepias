

-- newtype Pair a b = Pair   { getPair :: (a, b) } deriving (Show)
-- newtype Period a = Period { getPeriod :: (Pair a a) } deriving (Show)
--newtype PeriodInt = PeriodInt (Period Int) deriving (Show)


--period :: a -> a -> Period a
-- period a b = Period (Pair (a, b))

--instance Functor (Pair a b) where
--   fmap f (Pair (x, y)) = Pair (f (x, y))

-- start :: Period a -> a
-- start x = (fst.getPair.getPeriod) x

-- end :: Period a -> a
-- end x = (snd.getPair.getPeriod) x

--data Period' a = Either () (a, a)
--type Periodz  = Period' Int

data Periodz a = Periodz ( Either () (a, a) ) 
type PeriodInt = Periodz Int
{-
type Point    = Periodz
type Moment   = Periodz
type Interval = Periodz
-}

period :: Int -> Int -> Periodz Int
period a b 
  | b < a        =  Periodz () -- TODO: handle this in a more Haskelly way
  | a == b       =  Periodz Right (a, b)
  | b == (a + 1) =  Periodz Right (a, b)
  | otherwise    =  Periodz Right (a, b)
 
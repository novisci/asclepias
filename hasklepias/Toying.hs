import Data.Either

type Point = Int
newtype Begin = Begin { getBegin :: Point } 
newtype End   = End   { getEnd   :: Point } 

instance Show Begin where show x = show $ getBegin x
instance Show End where show x = show $ getEnd x
--newtype Pair a = Pair { getPair :: (a, a)} deriving (Show)
{-
instance Functor Pair where
   fmap f x = Pair (f $ fst p, f $ snd p) 
      where p = getPair x
-}
begin :: Period -> Point
begin x = getBegin $ fst $ getPeriod x

end :: Period -> Point
end x = getEnd $ snd $ getPeriod x

newtype Period = Period { getPeriod :: (Begin, End) } 

instance Show Period where
  show x = "(" ++ show (begin x) ++ ", " ++ show (end x) ++ ")"

--instance Functor Period where
--  fmap f x = Period $ fmap f (getPeriod x)

newtype SafePeriod = SafePeriod { getSafePeriod :: Either () Period }

instance Show SafePeriod where
  show x = either (\x -> "()") show $ getSafePeriod x

--instance Functor Safe where
--  fmap f (Left  (getSafe x)) = Safe $ Left $ ()
--   fmap f (Right (getSafe x)) = Safe $ Right $ f $ getSafe x
--   fmap f x = SafePeriod $ either (\x -> Left ()) (\x -> Right $ f $ x) z
--      where z = getSafePeriod x 

{-
instance Functor SafePeriod where
	fmap f (SafePeriod (Left ())) = SafePeriod $ Left ()
	fmap f (SafePeriod $ Right $ Period $ Pair (x, y)
-}
--newtype DiscretePeriod = DiscretePeriod (SafePeriod Int) deriving (Show)

period :: Int -> Int -> SafePeriod
period a b 
  | b < a        = SafePeriod $ Left ()
  | a == b       = SafePeriod $ Right $ Period $ (Begin a, End a)
  | a < b        = SafePeriod $ Right $ Period $ (Begin a, End b)



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
{- 
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
 -}
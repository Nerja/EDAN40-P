module Utilities where

-- | Function that takes two functions as a pair and applies them to one
--   argument each also given as a pair and returns the two results as a pair.
--
--   Examples:
--
--   >>> map2 (id, id) ("Foo", "Bar")
--   ("Foo","Bar")
--   >>> map2 ((^2),(^3)) (2, 2)
--   (4,8)
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- | Function that takes a function (a -> b) and tries to apply a argument.
--   The supplied argument is of maybe type and can therefore be nothing or a value
--   encapsulated inside Just. If the argument is Nothing then this function returns Nothing
--   otherwise the result of applying the given function to the given argument is returned
--   encapsulated inside Just.
--
--   Examples:
--
--   >>> mmap id Nothing
--   Nothing
--   >>> mmap id (Just "Marcus")
--   Just "Marcus"

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- | Takes two maybe argument and returns the first argument if it is not Nothing.
--   Returns the second argument if the first argument is Nothing. This means that
--   if both argument is Nothing then Nothing is returned.
--
--   Examples:
--
--   >>> orElse (Just "Andy") (Just "Marcus")
--   Just "Andy"
--   >>> orElse Nothing (Just "Marcus")
--   Just "Marcus"
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- | Takes a function and an argument. Tries to apply the given function to the
--   argument. If the function returns nothing then the supplied argument is returned
--   otherwise the result of the function application is returned.
--
--   Examples:
--
--   >>> try (\_ -> Nothing) "Hello, World"
--   "Hello, World"
--   >>> try (\s -> Just $ reverse s) "EDAN40"
--   "04NADE"
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- | Takes a function and an argument and computes the fixedpoint.
--
-- Examples:
--
-- >>> fix id "Stupid"
-- "Stupid"
-- >>> fix (/2) 10
-- 0.0
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- | Is like a selector that selects an element from the supplied list based
--   on the given parameter. The selection is done by thinking of the list as
--   the interval [0, 1[. If a value near 0 is picked then an element near the
--   beginning of the list will be returned. If a value near 1 is picked then an
--   element near the end of the list will be returned
--
--   Examples:
--
--   >>> pick 0.99 [1,2,3,4,5]
--   5
--   >>> pick 0 [1,2,3,4,5]
--   1
--   >>> pick 0.5 [1,2,3,4,5]
--   3
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

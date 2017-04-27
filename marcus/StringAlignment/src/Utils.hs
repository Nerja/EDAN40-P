module Utils (
  (.^)
) where

-- | Function decomposition of two functions f and g.
--   function g is applied first and should take two parameters.
--   Function f is applied to the result of function g.
--   Function comes from "http://buffered.io/posts/point-free-style-what-is-it-good-for/"
(.^) :: (b -> c) -> (a1 -> a -> b) -> a1 -> a -> c
(.^) = (.) . (.)

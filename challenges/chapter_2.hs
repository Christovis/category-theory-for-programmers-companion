{- Types and Functions -}

{-- Section: 2.7 --}

{--- 1. ---}
-- Define a higher-order function (or a function object) memoize in your favorite
-- language. This function takes a pure function f as an argument and returns a
-- function that behaves almost exactly like f, except that it only calls the
-- original function once for every argument, stores the result internally, and
-- subsequently returns this stored result every time it’s called with the same
-- argument. You can tell the memoized function from the original by watching
-- its performance. For instance, try to memoize a function that takes a long
-- time to evaluate. You’ll have to wait for the result the first time you call
-- it, but on subsequent calls, with the same argument, you should get the result
-- immediately.

-- Type signature
type Memoizer a = a

-- Identity morphism
return :: a -> Memoizer a
return x = (x, "")

-- Composite morphism
(<=<) :: (b -> Memoizer c) -> (a -> Memoizer b) -> (a -> Memoizer c)
m2 <=< m1 = \x ->
    let (y, s1) = m1 x
        (z, s2) = m2 y
    in (z, s2 ++ s1)


-- Example application
upCase :: String -> Memoizer String
upCase s = (map toUpper s, "upCase ")

toWords :: String -> Memoizer [String]
toWords s = (words s, "toWords ")

process :: String -> Memoizer [String]
process = toWords <=< upCase

{--- 2. ---}
-- Try to memoize a function from your standard library that you normally use to
-- produce random numbers. Does it work?
-- If it would work, it would be a random number generator.

{--- 3. ---}
-- Most random number generators can be initialized with a seed. Implement a
-- function that takes a seed, calls the random number generator with that seed,
-- and returns the result. Memoize that function. Does it work?

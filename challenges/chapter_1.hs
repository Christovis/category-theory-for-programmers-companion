{- 1. Category: The Essence of Composition -}


{-- Section: 1.4 --}


{--- 1. ---}
-- Implement, as best as you can, the identity function in your favorite
-- language (or the second favorite, if your favorite language happens to be Haskell).

-- Examples:
-- λ: :l chapter_1.hs
-- λ: identity 1.0
-- 1.0
-- λ: identity 3.14159265359
-- 3.14159265359
identity :: a -> a
identity x = x


{--- 2. ---}
-- Implement the composition function in your favorite language. It takes two
-- functions as arguments and returns a function that is their composition.

-- You can look up what signature our function 'compose' must have, by typing
-- ':t (.)' into ghci.
--
-- Examples:
-- λ: :l chapter_1.hs
-- λ: compose sqrt sqrt 1.0
-- 1.0
-- λ: compose (+1) (+1) 1.0
-- 3
compose ::  (b -> c) -> (a -> b) -> a -> c
compose g f = (g . f)


{--- 3. ---}
-- Write a program that tries to test that your composition function respects identity.

-- Examples:
-- $ runghc chapter_1.hs
-- $ "True"
-- $ "True"
main :: IO ()
main = do
  let test_a = compose sqrt identity 4.0 == 2.0
  let test_b = compose identity sqrt 4.0 == 2.0

  print $ show test_a
  print $ show test_b

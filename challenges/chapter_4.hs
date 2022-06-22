{- 4. Kleisli Categories -}

-- {-- Section: 4.4 --}

{--- 1. ---}
-- Construct the Kleisli category for partial functions
-- (define composition and identity).

-- An outline on how to answer this question is nicely given in [[PC]](#PC) Section 2.3.4, Example 2.50, page 54 and Chapter 5, page 125.

-- Haskell's Prelude comes with the `Maybe` type, with type signature
-- data Maybe a = Just a | Nothing

-- identity
identity :: a -> Maybe a
identity a = Just a

-- composition
(<=<) :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
g <=< f = \a -> case f a of
                   Nothing -> Nothing
                   Just b -> g b

{--- 2. ---}
-- b) Implement the embellished function `safe_reciprocal` that returns a valid reciprocal of its argument, if it's different from zero.

-- Instead of `safe_reciprocal` we will call the embellished function `safeDiv` as taking the reciprocal in Haskell is denoted as `div` (however, in Haskell `div` only takes Integers as arguments while `/` can take any Num type) and using a capital letter follows Haskell conventions.

-- Examples:
-- λ: :l chapter_4.hs
-- λ: div 8 0
-- *** Exception: divide by zero
-- λ: safeDiv 8 0
-- Nothing
-- λ: safeDiv 8 2
-- 4
safeDiv :: Float -> Float -> Maybe Float
safeDiv m n
        | n == 0    = Nothing
        | otherwise = Just (m / n)

{--- 3. ---}
-- c) Compose the functions `safe_root` and `safe_reciprocal` to implement `safe_root_reciprocal` that calculates `sqrt(1/x)` whenever possible.

safeDiv1 :: Float -> Maybe Float
safeDiv1 n
       | n == 0    = Nothing
       | otherwise = Just (1 / n)

-- Examples:
-- λ: :l chapter_4.hs
-- λ: sqrt (-2)
-- NaN
-- λ: safeSqrt (-2)
-- Nothing
safeSqrt :: Float -> Maybe Float
safeSqrt x
       | x >= 0    = Just (sqrt x)
       | otherwise = Nothing

-- Examples:
-- λ: :l chapter_4.hs
-- λ: safeSqrtDiv 2
-- Just 0.70710677
safeSqrtDiv :: Float -> Maybe Float
safeSqrtDiv = safeSqrt <=< safeDiv1

{--- References ---}
-- PC
-- 'Programming with Categories' by Brendan Fong, Bartosz Milewski, David I. Spivak

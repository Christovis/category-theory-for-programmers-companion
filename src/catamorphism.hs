cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

versionSum :: Fix Packet -> Integer
versionSum = cata $ \(Packet ver _ vers) -> ver + sum vers

eval :: Fix Packet -> Int
eval = cata $ \(Packet _ _ body) -> case body of
    Literal n -> n
    Sum xs -> sum xs
    Product xs -> product xs
    Minimum xs -> minimum xs
    Maximum xs -> maximum xs
    Greater x y -> if x > y then 1 else 0
    Less x y -> x < y then 1 else 0
    Equal x y -> x == y then 1 else 0

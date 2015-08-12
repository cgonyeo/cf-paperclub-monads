class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

instance Monad [a] where
    return x = [x]
    [] >>= f = []
    (x:xs) >>= f = (f x) ++ (xs >>= f)

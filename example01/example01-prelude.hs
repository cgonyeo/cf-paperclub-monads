class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

data Either a = Left a | Right a

instance Monad (Either e) where
    return = Right
    Left  l >>= _ = Left l
    Right r >>= k = k r

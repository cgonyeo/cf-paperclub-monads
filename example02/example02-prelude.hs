class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

data Maybe a = Just a | Nothing

instance Monad (Maybe a) where
    return = Just
    (Just v) >>= f = f v
    Nothing >>= _ = Nothing

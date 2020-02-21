module Cherry.Internal.Shortcut (map, map2, map3, map4, map5, map6, map7, map8, map9, andThen, afterwards, empty) where

import Prelude ((<*>), (>>=), Applicative, Monad, return)


{-| -}
andThen :: Monad m => (a -> m b) -> m a -> m b
andThen b a =
  a >>= b


{-| -}
afterwards :: Monad m => m b -> m a -> m b
afterwards b a =
  a >>= (\_ -> b)


{-| -}
map :: (Monad m, Applicative m) => (a -> value) -> m a -> m value
map f a =
  return f <*> a


{-| -}
map2 :: (Monad m, Applicative m) => (a -> b -> value) -> m a -> m b -> m value
map2 func a b =
  return func <*> a <*> b


{-| -}
map3 :: (Monad m, Applicative m) => (a -> b -> c -> value) -> m a -> m b -> m c -> m value
map3 func a b c =
  return func <*> a <*> b <*> c


{-| -}
map4 :: (Monad m, Applicative m) => (a -> b -> c -> d -> value) -> m a -> m b -> m c -> m d -> m value
map4 func a b c d =
  return func <*> a <*> b <*> c <*> d


{-| -}
map5 :: (Monad m, Applicative m) => (a -> b -> c -> d -> e -> value) -> m a -> m b -> m c -> m d -> m e -> m value
map5 func a b c d e =
  return func <*> a <*> b <*> c <*> d <*> e


{-| -}
map6 :: (Monad m, Applicative m) => (a -> b -> c -> d -> e -> f -> value) -> m a -> m b -> m c -> m d -> m e -> m f -> m value
map6 func a b c d e f =
  return func <*> a <*> b <*> c <*> d <*> e <*> f


{-| -}
map7 :: (Monad m, Applicative m) => (a -> b -> c -> d -> e -> f -> g -> value) -> m a -> m b -> m c -> m d -> m e -> m f -> m g -> m value
map7 func a b c d e f g =
  return func <*> a <*> b <*> c <*> d <*> e <*> f <*> g


{-| -}
map8 :: (Monad m, Applicative m) => (a -> b -> c -> d -> e -> f -> g -> h -> value) -> m a -> m b -> m c -> m d -> m e -> m f -> m g -> m h -> m value
map8 func a b c d e f g h =
  return func <*> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h


{-| -}
map9 :: (Monad m, Applicative m) => (a -> b -> c -> d -> e -> f -> g -> h -> i -> value) -> m a -> m b -> m c -> m d -> m e -> m f -> m g -> m h -> m i -> m value
map9 func a b c d e f g h i =
  return func <*> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i


{-| -}
empty :: Monad m => m ()
empty =
  return ()


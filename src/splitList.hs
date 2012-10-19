
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ z []         = return z
foldrM g z (x : xs)   = foldrM g z xs >>= g x


splitBy :: (Alternative f, Eq a) => [a] -> [a] -> [f a]
splitBy ks          = let ks' = reverse ks
                      in  fst
                            . flip runState ks'
                            . flip runReaderT ks'
                            . splitByM

splitByM :: (Alternative f, Eq a) =>
            [a] -> ReaderT [a] (State [a]) [f a]
splitByM xs         = do
                        (z1 : z2 : zs) <- foldrM fM [empty, empty] xs
                        return ((z1 <|> z2) : zs)
  where
    -- z1 is "probably separator", z2 is column behind the separator.
    --fM :: (Alternative f, Eq a) =>
    --      a -> [f a] -> ReaderT [a] (State [a]) [f a]
    fM x (z1 : z2 : zs) = do
                            ks <- ask
                            cs <- lift get
                            let (zs', cs') = f ks cs
                            lift (put cs')
                            return zs'
      where
        --f :: Eq a => [a] -> [a] -> ([f a], [a])
        f [] _          = (empty : (pure x <|> z2) : zs, [])
        f ks [c]
          | x == c      = (empty : empty : (pure x <|> z1) : z2 : zs, ks)
        f (k : ks) (c : cs)
          | x == c      = ((pure x <|> z1) : z2 : zs, cs)
          | x == k      = (pure x : (z1 <|> z2) : zs, ks)
          | otherwise   = (empty  : (pure x <|> z1 <|> z2) : zs, k : ks)

g :: a -> m (b, SepState)

liftM f (g x)

    f (x', Separator)
    f (x', ProbablySep)
    f (x', NewSeparator)
    f (x', NotSeparator)

separator
probablySeparator
newSeparator
notSeparator


foldrMerge :: (Monad m, Monoid b) => (a -> m (b, Bool)) -> [a] -> m [b]
foldrMerge g        = foldrM (\x zs -> liftM (f zs) (g x)) []
  where
    f :: (Monoid b) => [b] -> (b, Bool) -> [b]
    f [] (x', _)    = [x']
    f (z : zs) (x', p)
      | p           = x' `mappend` z : zs
      | otherwise   = x' : z : zs


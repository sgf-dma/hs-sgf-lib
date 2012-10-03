
import Control.Monad.Writer

wr  = runWriter (foldl (\mz x ->
        (if x == 3
           then listen mz >>= (\(z, w) -> return (getSum w))
           else mz) >>= \z ->
        Writer (z, Sum 1)) (return 0) [1..10])

wr' = runWriter (foldl (\mz x -> do
        z <- if x == 3
                then do 
                       (z, w) <- listen mz
                       return (getSum w)
                else mz
        tell (Sum 1)
        return z) (return 0) [1..10])

-- FIXME: Complete index implementation using Writer.
onlyElemsW :: (a -> Bool) -> a -> Writer (Sum Index) [Index] -> Writer (Sum Index) [Index]
onlyElemsW p x mz
  | p x             = do
                        (zs, w) <- listen mz
                        return (getSum w : zs)
  | otherwise       = mz

indsByElemsW :: (F.Foldable t, F.Foldable t1) =>
                (a -> a -> Bool) -> t1 a -> t a -> Writer (Sum Index) [Index]
indsByElemsW eq ks  = F.foldr (\x mz -> do
                                z' <- onlyElemsW p x mz
                                tell (Sum 1)
                                return z') (return [])
  where p x         = F.any (`eq` x) ks


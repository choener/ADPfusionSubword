
module ADPfusion.Subword.SynVar where

import qualified Data.Vector.Fusion.Stream.Monadic as SP

import Data.PrimitiveArray

import ADPfusion.Core
import ADPfusion.Subword.Core

-- | TODO cf with tables for subword
instance TermStaticVar (IStatic 0) (TwITbl bo so m arr c (Subword I) x) (Subword I) where
----{{{
  {-# Inline [0] termStreamIndex #-}
  termStreamIndex Proxy _ (Subword (i:.j)) = Subword (i:.j)
  {-# Inline [0] termStaticCheck #-}
  termStaticCheck Proxy _ _ _ grd = grd
----}}}

-- | TODO cf with tables for subword
instance TermStaticVar (IVariable d) (TwITbl bo so m arr c (Subword I) x) (Subword I) where
----{{{
  {-# Inline [0] termStreamIndex #-}
  termStreamIndex Proxy _ (Subword (i:.j)) = Subword (i:.j)
  {-# Inline [0] termStaticCheck #-}
  termStaticCheck Proxy _ _ _ grd = grd
----}}}

instance TermStaticVar (IStatic 0) (TwITblBt bo so arr c (Subword I) x mF mB r) (Subword I) where

instance TermStaticVar (IVariable 0) (TwITblBt bo so arr c (Subword I) x mF mB r) (Subword I) where

instance
  ( Monad m, PrimArrayOps arr (Subword I) x, TermStream m ps ts s is
  , TermStreamContext m ps ts s x0 i0 is (Subword I))
  => TermStream m (ps:.IStatic 0) (TermSymbol ts (TwITbl bo so m arr c (Subword I) x)) s (is:.Subword I) where
--{{{
  {-# Inline [1] termStream #-}
  termStream Proxy (ts:| TW (ITbl _ arr) f) (us:..LtSubword u) (is:.Subword (i:.j))
    = SP.map (\(TState s ii ee) ->
        let RiSwI l = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
            lj = subword l j
        in  TState s (ii:.:RiSwI j) (ee:.arr!lj))
    . termStream (Proxy :: Proxy ps) ts us is
--}}}

instance
  ( Monad m, PrimArrayOps arr (Subword I) x, TermStream m ps ts s is
  , TermStreamContext m ps ts s x0 i0 is (Subword I)
  ) => TermStream m (ps:.IVariable 0) (TermSymbol ts (TwITbl bo so m arr c (Subword I) x)) s (is:.Subword I) where
--{{{
  {-# Inline termStream #-}
  termStream Proxy (ts:| TW (ITbl _ arr) f) (us:..u) (is:.Subword (i:.j))
    = SP.flatten mk step . termStream (Proxy @ps) ts us is
    where
      {-# Inline [0] mk #-}
      mk tstate@(TState s ii ee) =
        let RiSwI l = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
        in  return (tstate, l, j-l)
      {-# Inline [0] step #-}
      step (TState s ii ee, k, z)
        | z >= 0 = do let l  = j-z; kl = subword k l
                      return $ SP.Yield (TState s (ii:.:RiSwI l) (ee:.arr!kl)) (TState s ii ee, k, z-1)
        | otherwise = return SP.Done
--}}}

instance
  (
  )
  => TermStream m (ps:.IStatic 0) (TermSymbol ts (TwITblBt bo so arr c (Subword I) x mF mB r)) s (is:.Subword I) where

instance
  (
  )
  => TermStream m (ps:.IVariable 0) (TermSymbol ts (TwITblBt bo so arr c (Subword I) x mF mB r)) s (is:.Subword I) where

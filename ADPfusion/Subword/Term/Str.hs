
module ADPfusion.Subword.Term.Str where

import           Data.Proxy
import           Data.Strict.Tuple
import           Data.Type.Equality
import           Debug.Trace
import           GHC.Exts
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Generic as VG

import           Data.PrimitiveArray

import           ADPfusion.Core
import           ADPfusion.Core.Term.Str
import           ADPfusion.Subword.Core



-- minSz done via TermStaticVar ?!

type instance LeftPosTy (IStatic   d) (Str linked minSz maxSz v x r) (Subword I) = IVariable d
type instance LeftPosTy (IVariable d) (Str linked minSz maxSz v x r) (Subword I) = IVariable d

{-
type instance LeftPosTy (OStatic d) (Chr r x) (PointL O) = OStatic (d+1)
-}

-- | 

instance
  forall pos posLeft m ls linked minSz maxSz v x r i
  . ( TermStream m (Z:.pos) (TermSymbol M (Str linked minSz maxSz v x r))
                 (Elm (Term1 (Elm ls (Subword i))) (Z :. Subword i)) (Z:.Subword i)
    , posLeft ~ LeftPosTy pos (Str linked minSz maxSz v x r) (Subword i)
    , TermStaticVar pos (Str linked minSz maxSz v x r) (Subword i)
    , MkStream m posLeft ls (Subword i)
    )
  => MkStream m pos (ls :!: Str linked minSz maxSz v x r) (Subword i) where
  mkStream pos (ls :!: str@(Str f xs)) grd us is
    = S.map (\(ss,ee,ii) -> ElmStr ee ii ss)
    . addTermStream1 pos (Str f xs `asTypeOf` str) us is
    $ mkStream (Proxy :: Proxy posLeft) ls
               (termStaticCheck pos (Str f xs `asTypeOf` str) us is grd)
               us (termStreamIndex pos (Str f xs `asTypeOf` str) is)
  {-# Inline mkStream #-}

class MaybeMaxSz (maxSz :: Maybe Nat) where
  maybeMaxSz :: Int -> a -> Maybe a
  gtMaxSz :: Int -> Bool

instance MaybeMaxSz Nothing where
  {-# Inline maybeMaxSz #-}
  maybeMaxSz _ = Just
  {-# Inline gtMaxSz #-}
  gtMaxSz _ = False

instance (KnownNat maxSz) => MaybeMaxSz (Just maxSz) where
  {-# Inline maybeMaxSz #-}
  maybeMaxSz k a
    | k <= maxSz = Just a
    | otherwise  = Nothing
    where maxSz = fromIntegral (natVal (Proxy :: Proxy maxSz))
  {-# Inline gtMaxSz #-}
  gtMaxSz k = k > fromIntegral (natVal (Proxy :: Proxy maxSz))

-- | Note that the @minSz@ should automatically work out due to the encoding in
-- @d@ / @termStaticVar@
--
-- NOTE The @maybeMaxSz@ check makes the assumption that in case of @maxSz == Nothing@, stream
-- fusion can fully elimiate the @S.mapMaybe / Just@ part. This should work, since it ends up being
-- case of known constructor.

instance
  ( TermStreamContext m ps ts s x0 i0 is (Subword I)
  , MaybeMaxSz maxSz
  ) => TermStream m (ps:.IStatic d) (TermSymbol ts (Str linked minSz maxSz v x r)) s (is:.Subword I) where
  termStream Proxy (ts:|Str f xs) (us:..LtSubword u) (is:.Subword (i:.j))
    = S.mapMaybe (\(TState s ii ee) -> let RiSwI l = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
        in maybeMaxSz @maxSz (j-l) $ TState s (ii:.:RiSwI j) (ee:.f l j xs) )
    . termStream (Proxy ∷ Proxy ps) ts us is
  {-# Inline termStream #-}



class LinkedSz (eqEmpty::Bool) (p::Symbol) ts i where
  linkedSz :: Elm ts i -> Int

-- | If the @Str@ we want to calculate for has a symbol @""@ then there is no need to sum up the
-- linked sizes, since "" declares independence.

instance LinkedSz True p any i where
  {-# Inline linkedSz #-}
  linkedSz _ = 0

-- | The linked name is non-empty, hence we now need to sum up linked sizes.

instance (eq ~ (p == linked), LinkedSzEq eq p (ls :!: Str linked minSz maxSz v x r) i )
  => LinkedSz False p (ls :!: Str linked minSz maxSz v x r) i where
  {-# Inline linkedSz #-}
  linkedSz ts = linkedSzEq @eq @p ts

instance LinkedSz eqEmpty linked (Term1 (Elm S (Subword I))) (Z:.Subword I) where
  {-# Inline linkedSz #-}
  linkedSz _ = 0



-- | This class calculates the actual link sizes.

class LinkedSzEq (eq::Bool) (p::Symbol) ts i where
  linkedSzEq :: Elm ts i -> Int

instance ( LinkedSz False p ls (Subword I), Element ls (Subword I) )
  => LinkedSzEq 'True p (ls :!: Str linked minSz maxSz v x r) (Subword I) where
  {-# Inline linkedSzEq #-}
  -- We have the correct type @Str@ and the same non-empty linked annotation. Extract the previous
  -- running index, and the current running index. Calculate their size difference, and recursively
  -- add more linked sizes.
  linkedSzEq (ElmStr _ (RiSwI j) ls) = let RiSwI i = getIdx ls in (j-i) + linkedSz @False @p ls

-- | This @Str@ does NOT have the same type-level string annotation

instance ( LinkedSz False p ls i )
  => LinkedSzEq 'False p (ls :!: Str linked minSz maxSz v x r) i where
  {-# Inline linkedSzEq #-}
  linkedSzEq (ElmStr _ _ ls) = linkedSz @False @p ls



-- |
--
-- X_ij -> S_ik Y_kj

instance
  ( TermStreamContext m ps ts s x0 i0 is (Subword I)
  , KnownNat minSz, KnownSymbol linked, MaybeMaxSz maxSz, LinkedSz (linked == "") linked x0 i0
  ) => TermStream m (ps:.IVariable d) (TermSymbol ts (Str linked minSz maxSz v x r)) s (is:.Subword I) where
  termStream Proxy (ts:|Str f xs) (us:..LtSubword u) (is:.Subword (i:.j))
    = S.flatten mk step . termStream (Proxy ∷ Proxy ps) ts us is
    where mk (tstate@(TState s ii ee)) =
            let RiSwI k = getIndex (getIdx s) (Proxy ∷ PRI is (Subword I))
                msz     = fromIntegral $ natVal (Proxy ∷ Proxy minSz)
            in  return (tstate,msz)
          step (TState s ii ee, sz)
            | k+sz > j || gtMaxSz @maxSz (lsz+sz) = return $ S.Done
            | otherwise = return $ S.Yield (TState s (ii:.:RiSwI ksz) (ee:.f k ksz xs))
                                           (TState s ii ee, sz+1)
            where RiSwI k = getIndex (getIdx s) (Proxy ∷ PRI is (Subword I))
                  ksz = k+sz
                  lsz = linkedSz @(linked == "") @linked s
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline termStream #-}

instance (KnownNat minSz)
  ⇒ TermStaticVar (IStatic d) (Str linked minSz maxSz v x r) (Subword I) where
  termStreamIndex Proxy (Str _ _) (Subword (i:.j)) = subword i $ j - fromIntegral (natVal (Proxy ∷ Proxy minSz))
  termStaticCheck Proxy (Str _ _) _ _ grd = grd
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}

instance (KnownNat minSz)
  => TermStaticVar (IVariable d) (Str linked minSz maxSz v x r) (Subword I) where
  termStreamIndex Proxy (Str _ _) (Subword (i:.j)) = subword i $ j - fromIntegral (natVal (Proxy ∷ Proxy minSz))
  termStaticCheck Proxy (Str _ _) _ _ grd = grd
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}

{-
instance TermStaticVar (OStatic d) (Chr r x) (PointL O) where
  termStreamIndex Proxy (Chr f x) (PointL j) = PointL $ j
  termStaticCheck Proxy (Chr f x) (PointL j) = 1#
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}
-}



module ADP.Fusion.Subword.Term.Str where

import           Data.Proxy
import           Data.Strict.Tuple
import           Debug.Trace
import           GHC.Exts
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Generic as VG

import           Data.PrimitiveArray

import           ADP.Fusion.Core
import           ADP.Fusion.Core.Term.Str
import           ADP.Fusion.Subword.Core



-- minSz done via TermStaticVar ?!

type instance LeftPosTy (IStatic   d) (Str linked minSz maxSz v x) (Subword I) = IVariable d
type instance LeftPosTy (IVariable d) (Str linked minSz maxSz v x) (Subword I) = IVariable d

{-
type instance LeftPosTy (OStatic d) (Chr r x) (PointL O) = OStatic (d+1)
-}

-- | 

instance
  forall pos posLeft m ls linked minSz maxSz v x i
  . ( TermStream m (Z:.pos) (TermSymbol M (Str linked minSz maxSz v x))
                 (Elm (Term1 (Elm ls (Subword i))) (Z :. Subword i)) (Z:.Subword i)
    , posLeft ~ LeftPosTy pos (Str linked minSz maxSz v x) (Subword i)
    , TermStaticVar pos (Str linked minSz maxSz v x) (Subword i)
    , MkStream m posLeft ls (Subword i)
    )
  ⇒ MkStream m pos (ls :!: Str linked minSz maxSz v x) (Subword i) where
  mkStream pos (ls :!: Str xs) grd us is
    = S.map (\(ss,ee,ii) -> ElmStr ee ii ss) -- recover ElmChr
    . addTermStream1 pos (Str @linked @minSz @maxSz xs) us is
    $ mkStream (Proxy ∷ Proxy posLeft) ls
               (grd `andI#` termStaticCheck pos (Str @linked @minSz @maxSz xs) is)
               us (termStreamIndex pos (Str @linked @minSz @maxSz xs) is)
  {-# Inline mkStream #-}

-- | Note that the @minSz@ should automatically work out due to the encoding in
-- @d@.

instance
  ( TermStreamContext m ps ts s x0 i0 is (Subword I)
  ) ⇒ TermStream m (ps:.IStatic d) (TermSymbol ts (Str Nothing minSz Nothing v x)) s (is:.Subword I) where
  termStream Proxy (ts:|Str xs) (us:..LtSubword u) (is:.Subword (i:.j))
    = S.map (\(TState s ii ee) ->
                let RiSwI l = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
                in  TState s (ii:.:RiSwI j) (ee:.VG.unsafeSlice l (j-l) xs) )
    . termStream (Proxy ∷ Proxy ps) ts us is

{-
instance
  ( TermStreamContext m ps ts s x0 i0 is (PointL O)
  ) => TermStream m (ps:.OStatic d) (TermSymbol ts (Chr r x)) s (is:.PointL O) where
  termStream Proxy (ts:|Chr f xs) (us:..LtPointL u) (is:.PointL i)
    = S.map (\(TState s ii ee) ->
                let RiPlO k o = getIndex (getIdx s) (Proxy :: PRI is (PointL O))
                in  TState s (ii:.: RiPlO (k+1) o) (ee:.f xs k))
    . termStream (Proxy ∷ Proxy ps) ts us is
  {-# Inline termStream #-}
-}

{-
instance (KnownNat minSz)
  ⇒ TermStaticVar (IStatic d) (Str Nothing minSz Nothing v x) (PointL I) where
  termStreamIndex Proxy (Str xs) (PointL j) = PointL $ j - fromIntegral (natVal (Proxy ∷ Proxy minSz))
  termStaticCheck Proxy (Str xs) (PointL j) = 1#
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}
-}

{-
instance TermStaticVar (OStatic d) (Chr r x) (PointL O) where
  termStreamIndex Proxy (Chr f x) (PointL j) = PointL $ j
  termStaticCheck Proxy (Chr f x) (PointL j) = 1#
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}
-}



-- | TODO migrate instances to correct modules
--
-- TODO need to find out if we can reduce the total number of instances
-- required here. Probably not trivial since there are, in principle, @n*m@
-- instances that we need to handle.

module ADPfusion.SynVar.Array.TermSymbol.Subword where

import Data.Proxy
import Data.Strict.Tuple hiding (snd)
import Data.Vector.Fusion.Util (delay_inline)
import Data.Vector.Fusion.Stream.Monadic hiding (flatten)
import Debug.Trace
import Prelude hiding (map,mapM)

import Data.PrimitiveArray hiding (map)

import ADPfusion.Core
import ADPfusion.Core.Subword



-- |
--
-- TODO need to handle @minSize@ conditions!

instance
  ( TstCtx m ts s x0 i0 is (Subword I)
  , PrimArrayOps arr (Subword I) x
  ) => TermStream m (TermSymbol ts (TwITbl m arr c (Subword I) x)) s (is:.Subword I) where
  --
  termStream (ts:|TW (ITbl _ _ _ t) _) (cs:.IStatic ()) (us:.u) (is:.Subword (i:.j))
    = map (\(TState s ii ee) ->
              let RiSwI l = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
                  lj      = subword l j
              in  TState s (ii:.:RiSwI j) (ee:.t!lj) )
    . termStream ts cs us is
  --
  termStream (ts:|TW (ITbl _ _ _ t) _) (cs:.IVariable ()) (us:.u) (is:.Subword (i:.j))
    = flatten mk step . termStream ts cs us is
    where mk tstate@(TState s ii ee) =
              let RiSwI l = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
              in  return (tstate, l, j - l)
          step (tstate@(TState s ii ee), k, z)
            | z >= 0 = do let l  = j - z
                              kl = subword k l
                          return $ Yield (TState s (ii:.:RiSwI l) (ee:.t!kl)) (tstate, k, z-1)
            | otherwise = return $ Done
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline termStream #-}

-- |
--
-- TODO can we combine the @ITbl@ and @BtITbl@ code again?

instance
  ( TstCtx mB ts s x0 i0 is (Subword I)
  , PrimArrayOps arr (Subword I) x
  ) => TermStream mB (TermSymbol ts (TwITblBt arr c (Subword I) x mF mB r)) s (is:.Subword I) where
  termStream (ts:|TW (BtITbl c t) bt) (cs:.IStatic ()) (us:.u) (is:.Subword (i:.j))
    = mapM (\(TState s ii ee) ->
                let RiSwI l = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
                    lj      = subword l j
                in  bt u lj >>= \ ~bb -> return $ TState s (ii:.:RiSwI j) (ee:.(t!lj,bb)) )
    . termStream ts cs us is
  termStream (ts:|TW (BtITbl c t) bt) (cs:.IVariable ()) (us:.u) (is:.Subword (i:.j))
    = flatten mk step . termStream ts cs us is
    where mk tstate@(TState s ii ee) =
              let RiSwI l = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
              in  return (tstate, l, j - l)
          step (tstate@(TState s ii ee), k, z)
            | z >= 0 = do let l  = j - z
                              kl = subword k l
                          bt u kl >>= \ ~bb -> return $ Yield (TState s (ii:.:RiSwI l) (ee:.(t!kl,bb))) (tstate, k, z-1)
            | otherwise = return $ Done
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline termStream #-}

-- | TODO need to deal with @minSize@

--instance
--  ( Monad m
--  , TerminalStream m a is
--  , PrimArrayOps arr (Subword I) x
--  , Show x
--  ) => TerminalStream m (TermSymbol a (ITbl m arr (Subword I) x)) (is:.Subword I) where
--  terminalStream (a :| ITbl _ _ c t _) (sv:.IStatic _) (is:.ix@(Subword (i:.j)))
--    = map (\ (S6 s (zi:.(Subword (a:.l))) (zo:._) is os e) ->
--              let lj = subword l j
--              in  {- traceShow (i,a,' ',l,j,t!lj) $ -} S6 s zi zo (is:.lj) (os:.subword 0 0) (e:.(t!lj)) )
--    . iPackTerminalStream a sv (is:.ix)
--  terminalStream (a :| ITbl _ _ c t _) (sv:.IVariable _) (is:.ix@(Subword (i:.j)))
--    = flatten mk step . iPackTerminalStream a sv (is:.ix)
--    where mk (S6 s (zi:.(Subword (_:.l))) (zo:._) is os e) = return (S6 s zi zo is os e :. l :. j - l) -- TODO minsize c !
--          step (s6:.k:.z) | z >= 0 = do let S6 s zi zo is os e = s6
--                                            l                  = j - z
--                                            kl                 = subword k l
--                                        return $ Yield (S6 s zi zo (is:.kl) (os:.subword 0 0) (e:.(t!kl))) (s6 :. k :. z-1)
--                          | otherwise = return $ Done
--          {-# Inline [0] mk   #-}
--          {-# Inline [0] step #-}
--  {-# Inline terminalStream #-}

--instance
--  ( Monad mB
--  , TerminalStream mB a is
--  , PrimArrayOps arr (Subword I) x
--  ) => TerminalStream mB (TermSymbol a (Backtrack (ITbl mF arr (Subword I) x) mF mB r)) (is:.Subword I) where
--  terminalStream (a :| BtITbl c t bt) (sv:.IStatic _) (is:.ix@(Subword (i:.j)))
--    = mapM (\ (S6 s (zi:.(Subword (_:.l))) (zo:._) is os e) ->
--              let lj = subword l j
--                  hh = snd $ bounds t
--              in  bt hh lj >>= \ ~bb -> return $ S6 s zi zo (is:.lj) (os:.subword 0 0) (e:.(t!lj, bb)) )
--    . iPackTerminalStream a sv (is:.ix)
--  terminalStream (a :| BtITbl c t bt) (sv:.IVariable _) (is:.ix@(Subword (i:.j)))
--    = flatten mk step . iPackTerminalStream a sv (is:.ix)
--    where mk (S6 s (zi:.(Subword (_:.l))) (zo:._) is os e) = return (S6 s zi zo is os e :. l :. j - l) -- TODO minsize c !
--          step (s6:.k:.z) | z >= 0 = do let S6 s zi zo is os e = s6
--                                            l                  = j - z
--                                            kl                 = subword k l
--                                            hh                 = snd $ bounds t
--                                        bt hh kl >>= \ ~bb -> return $ Yield (S6 s zi zo (is:.kl) (os:.subword 0 0) (e:.(t!kl,bb))) (s6 :. k :. z-1)
--                          | otherwise = return $ Done
--          {-# Inline [0] mk   #-}
--          {-# Inline [0] step #-}
--  {-# Inline terminalStream #-}


instance TermStaticVar (TwITbl m arr c (Subword I) x) (Subword I) where
  termStaticVar _ (IStatic   d) _ = IVariable d
  termStaticVar _ (IVariable d) _ = IVariable d
  termStreamIndex (TW (ITbl _ _ _ _) _) (IStatic   d) (Subword (i:.j)) = subword i j -- TODO minSize handling !
  termStreamIndex (TW (ITbl _ _ _ _) _) (IVariable d) (Subword (i:.j)) = subword i j -- TODO minsize handling
  {-# Inline [0] termStaticVar   #-}
  {-# Inline [0] termStreamIndex #-}

instance TermStaticVar (TwITblBt arr c (Subword I) x mF mB r) (Subword I) where
  termStaticVar _ (IStatic   d) _ = IVariable d
  termStaticVar _ (IVariable d) _ = IVariable d
  termStreamIndex (TW (BtITbl _ _) _) (IStatic   d) (Subword (i:.j)) = subword i j -- TODO minSize handling !
  termStreamIndex (TW (BtITbl _ _) _) (IVariable d) (Subword (i:.j)) = subword i j -- TODO minsize handling
  {-# Inline [0] termStaticVar   #-}
  {-# Inline [0] termStreamIndex #-}


{-
  mkStream (ls :!: ITbl _ _ c t _) (IVariable ()) hh (Subword (i:.j))
    = flatten mk step Unknown $ mkStream ls (IVariable ()) hh (delay_inline Subword (i:.j - minSize c))
    where mk s = let Subword (_:.l) = getIdx s in return (s :. j - l - minSize c)
          step (s:.z) | z >= 0 = do let Subword (_:.k) = getIdx s
                                        l              = j - z
                                        kl             = subword k l
                                    return $ Yield (ElmITbl (t ! kl) kl (subword 0 0) s) (s:. z-1)
                      | otherwise = return $ Done

  terminalStream (a:|Chr f v) (sv:.IVariable _) (is:.ix@(Subword (i:.j)))
    = S.map (\(S6 s (zi:.Subword (_:.l)) (zo:._) is os e) -> S6 s zi zo (is:.subword l (l+1)) (os:.subword 0 0) (e:.f v l))
    . iPackTerminalStream a sv (is:.ix)
  {-# Inline terminalStream #-}

instance TermStaticVar (Chr r x) Subword where
  termStaticVar _ sv _ = sv
  termStreamIndex _ _ (Subword (i:.j)) = subword i (j-1)
  {-# Inline [0] termStaticVar   #-}
  {-# Inline [0] termStreamIndex #-}

-}


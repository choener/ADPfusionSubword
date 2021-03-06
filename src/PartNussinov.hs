
-- | Nussinovs RNA secondary structure prediction algorithm via basepair
-- maximization.

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Char (toUpper,toLower)
import           Data.List
import           Data.Vector.Fusion.Util
import           Debug.Trace
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Numeric.Log as Log
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           System.Environment (getArgs)
import           Text.Printf

import           Data.PrimitiveArray as PA

import           ADP.Fusion.Subword



-- * Inside and Outside grammar constructs

data Nussinov m c e x r = Nussinov
  { unp :: x -> c -> x
  , jux :: x -> x -> x
  , pai :: c -> x -> c -> x
  , nil :: e -> x
  , h   :: SM.Stream m x -> m r
  }

makeAlgebraProduct ''Nussinov

bpmax :: Monad m => Nussinov m Char () Int Int
bpmax = Nussinov
  { unp = \ x c   -> x
  , jux = \ x y   -> x + y
  , pai = \ c x d -> if c `pairs` d then x+1 else (-999999)
  , nil = \ ()    -> 0
  , h   = SM.foldl' max (-999999)
  }
{-# INLINE bpmax #-}

prob :: Monad m => Nussinov m Char () (Log Double) (Log Double)
prob = Nussinov
  { unp = \ x c     -> 0.1 * x                                -- 'any'
  , jux = \ x y     -> 0.9 * x * y                            -- 'any'
  , pai = \ c x d   -> 1.0 * if c `pairs` d then x else 0     -- 'paired'
  , nil = \ ()      -> 1.0                                    -- 'any'
  , h   = SM.foldl' (+) 0
  }
{-# Inline prob #-}

-- |

pairs !c !d
  =  c=='A' && d=='U'
  || c=='C' && d=='G'
  || c=='G' && d=='C'
  || c=='G' && d=='U'
  || c=='U' && d=='A'
  || c=='U' && d=='G'
{-# INLINE pairs #-}

pretty :: Monad m => Nussinov m Char () String (SM.Stream m String)
pretty = Nussinov
  { unp = \ x c     -> x ++ "."
  , jux = \ x y     -> x ++ y
  , pai = \ c x d   -> "(" ++ x ++ ")"
  , nil = \ ()      -> ""
  , h   = return . id
  }
{-# INLINE pretty #-}

-- | The inside grammar is:
--
-- @
-- A -> A c
-- A -> A P
-- A -> ε
-- P -> c A c
-- @

-- insideGrammar :: Nussinov m Char () x r -> c' -> t' -> (t', Subword -> m r)
insideGrammar Nussinov{..} c a' p' =
  let a = TW a' ( unp <<< a % c     |||
                  jux <<< a % p     |||
                  nil <<< Epsilon   ... h
                )
      p = TW p' ( pai <<< c % a % c ... h
                )
  in Z:.p:.a
{-# INLINE insideGrammar #-}

-- | Given the inside grammar, the outside grammar is:
--
-- @
-- B -> B c
-- B -> B P
-- B -> ε
-- B -> c Q c
-- Q -> A B
-- @

outsideGrammar Nussinov{..} !c !a !p !b' !q' =
  let b = TW b' ( unp <<< b % c         |||
                  jux <<< b % p         |||
                  pai <<< c % q % c     |||
                  nil <<< Epsilon       ... h
                )
      q = TW q' ( jux <<< a % b         ... h
                )
  in Z:.b:.q
{-# INLINE outsideGrammar #-}



-- * Ensemble collection constructs

data NussinovEnsemble m v ci x r = NussinovEnsemble
  { ens :: v -> ci -> v -> x
  , hhh :: SM.Stream m x -> m r
  }

ensemble
  :: Monad m
  => Log Double
  -> NussinovEnsemble
        m
        (Log Double)
        (Subword C)
        (Subword C, Log Double)
        [(Subword C, Log Double)]
ensemble z = NussinovEnsemble
  { ens = \ x i y -> ( i , x * y / z )
  , hhh = SM.toList
  }
{-# Inline ensemble #-}

ensembleGrammar NussinovEnsemble{..} !i !o !v' =
  let v = TW v' ( ens <<< i % (PeekIndex :: PeekIndex (Subword C)) % o ... hhh )
  in  Z:.v
{-# Inline ensembleGrammar #-}

-- makeAlgebraProductH ['hhh] ''NussinovEnsemble



-- * Run different algorithm parts

runNussinov :: String -> ([(Subword C, Log Double)], Log Double, [(Int,Int, Log Double, Log Double, Log Double, Log Double)])
runNussinov inp = (es,z,ys) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  !(Z:.p:.a) = runInsideForward i
  !(Z:.b:.q) = runOutsideForward i a p
  es = runEnsembleForward z p q
  za = let (TW (ITbl _ _ _ arr) _) = a in arr PA.! subword 0 n
  zp = let (TW (ITbl _ _ _ arr) _) = p in arr PA.! subword 0 n
  z  = za
  e = let (TW (ITbl _ _ _ arr) _) = b in Log.sum [ arr PA.! (subword k k) | k <- [0 .. n] ]
  ys =  [ ( k
          , l
          , fwda PA.! subword k l
          , fwdp PA.! subword k l
          , bwdb PA.! subword k l
          , bwdq PA.! subword k l
          )
        | let (TW (ITbl _ _ _ fwda) _) = a
        , let (TW (ITbl _ _ _ fwdp) _) = p
        , let (TW (ITbl _ _ _ bwdb) _) = b
        , let (TW (ITbl _ _ _ bwdq) _) = q
        , k <- [0 .. n]
        , l <- [k .. n]
        ]
{-# NOINLINE runNussinov #-}

neat :: String -> IO ()
neat i = do let (es,z,ys) = runNussinov i
            forM_ ys $ \ (k,_,_,_,_,_) -> printf " %6d" k
            putStrLn ""
            forM_ ys $ \ (_,l,_,_,_,_) -> printf " %6d" l
            putStrLn ""
            forM_ ys $ \ (_,_,a,_,_,_) -> printf " %0.4f" (exp $ ln a)
            putStrLn ""
            forM_ ys $ \ (_,_,_,p,_,_) -> printf " %0.4f" (exp $ ln p)
            putStrLn ""
            forM_ ys $ \ (_,_,_,_,b,_) -> printf " %0.4f" (exp $ ln b)
            putStrLn ""
            forM_ ys $ \ (_,_,_,_,_,q) -> printf " %0.4f" (exp $ ln q)
            putStrLn ""
            printf "%0.4f\n" $ exp $ ln z
            forM_ ys $ \ (_,_,_,p,_,q) -> printf " %0.4f" ((exp $ ln p) * (exp $ ln q) / (exp $ ln z))
            putStrLn ""
            putStrLn ""
            forM_ es $ \ (Subword (i:.j),v) -> printf "%3d %3d  %0.4f\n" i j (exp $ ln v)
            putStrLn ""

type TblI = TwITbl Id Unboxed EmptyOk (Subword I) (Log Double)
type TblO = TwITbl Id Unboxed EmptyOk (Subword O) (Log Double)

runInsideForward :: VU.Vector Char -> Z:.TblI:.TblI
runInsideForward i = mutateTablesDefault
                   $ insideGrammar prob
                       (chr i)
                       (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) 0 []))
                       (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) 0 []))
  where n = VU.length i
{-# NoInline runInsideForward #-}

runOutsideForward :: VU.Vector Char -> TblI -> TblI -> Z:.TblO:.TblO
runOutsideForward i a p = mutateTablesDefault
                        $ outsideGrammar prob
                            (chr i)
                            a p
                            (ITbl 0 0 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) 0 []))
                            (ITbl 0 1 EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) 0 []))
  where n = VU.length i
{-# NoInline runOutsideForward #-}

runEnsembleForward :: Log Double -> TblI -> TblO -> [ (Subword C,Log Double) ]
runEnsembleForward z i o = unId $ axiom g
  where (Z:.g) = ensembleGrammar (ensemble z)
                   i o
                   (IRec EmptyOk (Subword l) (Subword h))
                 :: Z :. TwIRec Id EmptyOk (Subword C) [(Subword C, Log Double)]
        (Subword l,Subword h) = let (TW (ITbl _ _ _ arr) _) = i in bounds arr
{-# NoInline runEnsembleForward #-}

{-
runPartitionNussinov :: String -> [(Subword,Double,Double,Double)]
runPartitionNussinov inp
  = Data.List.map (\(sh,a) -> let b = iTblArray t PA.! (O sh)
                              in (sh, a, b, a*b/d)
                  ) (PA.assocs $ iTblArray s)
  where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  s :: ITbl Id Unboxed Subword Double
  !(Z:.s) = mutateTablesDefault
          $ grammar prob
              (chr i)
              (ITbl EmptyOk (PA.fromAssocs (subword 0 0) (subword 0 n) 0 []))
              
  d = iTblArray s PA.! subword 0 n
  t :: ITbl Id Unboxed (Outside Subword) Double
  !(Z:.t) = mutateTablesDefault
          $ outsideGrammar prob
              (chr i)
              --(undefined :: ITbl Id Unboxed (Outside Subword) Double)
              s
              (ITbl EmptyOk (PA.fromAssocs (O $ subword 0 0) (O $ subword 0 n) (-1) []))
{-# NOINLINE runPartitionNussinov #-}
-}

main :: IO ()
main = do
  return ()
  {-
  as <- getArgs
  let k = if null as then 1 else read $ head as
  ls <- lines <$> getContents
  forM_ ls $ \l -> do
    putStrLn l
    let (s,xs) = runNussinov k l
    mapM_ (\x -> printf "%s %5d\n" x s) xs
  -}


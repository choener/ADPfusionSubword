
{-# Options_GHC -fdicts-cheap                  #-}
{-# Options_GHC -flate-dmd-anal                #-}
{-# Options_GHC -fmax-worker-args=1000         #-}
{-# Options_GHC -fspec-constr-count=20000      #-}
{-# Options_GHC -fspec-constr-keen             #-}
{-# Options_GHC -fspec-constr-recursive=20000  #-}
{-# Options_GHC -fspec-constr-threshold=1000   #-}
{-# Options_GHC -Wno-partial-type-signatures   #-}
-- both, full laziness and no liberate case are essential to have things inline nicely!
{-# Options_GHC -fno-full-laziness             #-}
{-# Options_GHC -fno-liberate-case             #-}

{-# Language RecordWildCards #-}
{-# Language NoMonomorphismRestriction #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Char (toUpper,toLower)
import           Data.List as L
import           Data.Vector.Fusion.Util
import           Debug.Trace
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           System.Environment (getArgs)
import           Text.Printf

import           Data.PrimitiveArray as PA

import           ADPfusion.Subword
import           ADPfusion.Core



data Tup m x r c = Tup
  { pkk :: (Z:.c) -> x
  , nll :: (Z:.()) -> x
  , h   :: SM.Stream m x -> m r
  }

makeAlgebraProduct ''Tup


bpmax :: Monad m => Tup m Int Int Char
bpmax = Tup
  { pkk = \ (Z:.c) -> 123456
  , nll = \ (Z:.()) -> 987654
  , h   = SM.foldl' max (-999999)
  }
{-# INLINE bpmax #-}

-- |

grammar Tup{..} u' c =
  let u = TW u' ( pkk <<< (M:|c)       |||
                  nll <<< (M:|Epsilon @Global) ... h
                )
  in Z:.u
{-# INLINE grammar #-}


type T = TwITbl 0 0 Id (Dense VU.Vector) (Z:.EmptyOk) (Z:.Subword I) Int

runInsideForward :: VU.Vector Char -> Mutated (Z:.T)
runInsideForward i = runST $ do
  let n = VU.length i
  arr <- newWithPA (ZZ:..LtSubword n) (-888888)
  let guideIndex = Z:.BOI @0 (upperBound arr)
  fillTablesDim guideIndex $ grammar bpmax
    (ITbl @_ @_ @_ @_ @_ @_ (Z:.EmptyOk) arr)
    (chr i)
  where n = VU.length i
{-# NoInline runInsideForward #-}

main :: IO ()
main = do
  return ()
--  as <- getArgs
--  let k = if null as then 1 else read $ head as
--  ls <- lines <$> getContents
--  forM_ ls $ \l -> do
--    putStrLn l
--    let (s,xs) = runPseudoknot k l
--    print s
--    mapM_ (\[x] -> printf "%s %5d\n" x s) xs


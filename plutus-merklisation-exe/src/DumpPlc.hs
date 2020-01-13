{- Load the ASTs for the validators of the sample contracts and print
   out a markdown table showing how many of each type of node there
   are, and what percentage of the total number of nodes these make
   up. Also print out a second table showing depths of entire AST and
   depth of nesting of Lams. -}

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module DumpPlc (main) where

import           Language.PlutusCore                                           as PLC
import           Language.PlutusCore.CBOR
import qualified Language.PlutusCore.Erasure.Untyped.Convert                   as C
import qualified Language.PlutusCore.Erasure.Untyped.Instance.Pretty           ()
import qualified Language.PlutusCore.Erasure.Untyped.Term                      as U
import           Language.PlutusCore.Merkle.Merklisable
import qualified Language.PlutusCore.Merkle.Merklise                           as M

import           Language.PlutusTx.Coordination.Contracts.Crowdfunding         as Crowdfunding
import           Language.PlutusTx.Coordination.Contracts.Currency             as Currrency
import           Language.PlutusTx.Coordination.Contracts.Escrow               as Escrow
import           Language.PlutusTx.Coordination.Contracts.Future               as Future
import           Language.PlutusTx.Coordination.Contracts.Game                 as Game
import           Language.PlutusTx.Coordination.Contracts.GameStateMachine     as GameStateMachine
import           Language.PlutusTx.Coordination.Contracts.MultiSig             as MultiSig
import           Language.PlutusTx.Coordination.Contracts.MultiSigStateMachine as MultiSigStateMachine
import           Language.PlutusTx.Coordination.Contracts.PubKey               as PubKey
import           Language.PlutusTx.Coordination.Contracts.Swap                 as Swap
import           Language.PlutusTx.Coordination.Contracts.TokenAccount         as TokenAccount
import           Language.PlutusTx.Coordination.Contracts.Vesting              as Vesting

import           Language.PlutusCore.Pretty
import qualified Language.PlutusTx                                             as PlutusTx

import           Codec.CBOR.FlatTerm                                           (toFlatTerm)
import qualified Codec.CBOR.Write                                              as Write
import           Codec.Serialise                                               (Serialise, deserialise, encode,
                                                                                serialise)
import           Control.Monad.State
import           Crypto.Hash
import qualified Numeric

import qualified Codec.Compression.GZip                                        as G
import qualified Data.Bits                                                     as Bits
import qualified Data.ByteArray                                                as BA
import qualified Data.ByteString.Lazy                                          as BSL

compress :: BSL.ByteString -> BSL.ByteString
compress = G.compressWith G.defaultCompressParams {G.compressLevel = G.bestCompression}

{-
numberTerm :: Term tyname name ->

numberProgram2 :: Program tyname name () -> Program tyname name Integer
numberProgram2 (Program () (Version () a b c) body) =
    do
      body' <- runState numberTerm body
      pure $ Program 0 (Version 1 a b c) body'
-}

makeEdges :: Integer -> Term tyname name Integer -> IO ()
makeEdges n term = do
  let h = termLoc term
  putStrLn $ show n ++ " -> " ++ show h
  do
    case term of
        Var      {}             -> pure ()
        TyAbs    _x _tn _k t    -> makeEdges h t
        LamAbs   _x _n _ty t    -> makeEdges h t
        Apply    _x t1 t2       ->
            do
              makeEdges h t1
              makeEdges h t2
        Constant _x _c          -> pure ()
        Builtin  _x _b          -> pure ()
        TyInst   _x t _ty       -> makeEdges h t
        Unwrap   _x t           -> makeEdges h t
        IWrap    _x _ty1 _ty2 t -> makeEdges h t
        Error _x _ty            -> pure ()

makeEdgesUntyped :: Integer -> U.Term name Integer -> IO ()
makeEdgesUntyped n term = do
  let h = U.termLoc term
  putStrLn $ show n ++ " -- " ++ show h
  do
    case term of
        U.Var      {}          -> pure ()
        U.LamAbs   _x _n t     -> makeEdgesUntyped h t
        U.Apply    _x t1 t2    ->
            do
              makeEdgesUntyped h t1
              makeEdgesUntyped h t2
        U.Constant _x _c       -> pure ()
        U.Builtin  _x _b       -> pure ()
        U.Error _x             -> pure ()
        U.Prune _x _h          -> pure ()



makeGraph ::  String -> PlutusTx.CompiledCode a -> IO ()
makeGraph name code = do
    let program = PlutusTx.getPlc code
        U.Program _ _ body = C.erasePLCProgram . M.numberProgram $ program
    putStrLn "graph {"
    putStrLn "   node[label=\"\", shape=point, color=red]"
    makeEdgesUntyped 0 body
    putStrLn "}"


dumpFlatCode :: PlutusTx.CompiledCode a -> IO ()
dumpFlatCode prg = do
  let v = toFlatTerm . encode $ PlutusTx.getPlc prg
  mapM_ (putStrLn . show) v

dumpBSL :: BSL.ByteString -> IO ()
dumpBSL bs = mapM_ (\b -> putStrLn (show b)) (BSL.unpack bs)


dumpSerialised :: PlutusTx.CompiledCode a -> IO ()
dumpSerialised code = do
  let program = PlutusTx.getPlc code
      sp = serialise program
  BSL.putStr sp

dumpSerialisedBytes :: PlutusTx.CompiledCode a -> IO ()
dumpSerialisedBytes code = do
  let program = PlutusTx.getPlc code
      sp = serialise program
  dumpBSL sp

countBitsBSL :: BSL.ByteString -> (Int, Int, Float)
countBitsBSL sp =
  let numbits = fromIntegral $ 8 * BSL.length sp
      numones = BSL.foldl (\n b -> Bits.popCount b + n) 0 sp
      density = ((fromIntegral numones) / (fromIntegral numbits)) :: Float
  in  (numbits, numones, density)


countBits :: PlutusTx.CompiledCode a -> IO ()
countBits code = do
    let program = PlutusTx.getPlc code
        sp = serialise program
        (numbits, numones, density) = countBitsBSL sp
    putStrLn $ show (BSL.length sp) ++ " bytes, " ++ show numbits ++ " bits, " ++ show numones ++ " set"
             ++ " (" ++ Numeric.showFFloat (Just 2) (8*density) "" ++ " bits/byte)"

countBitsProg :: U.Program C.IntName () -> IO ()
countBitsProg program = do
    let sp = serialise program
        (numbits, numones, density) = countBitsBSL sp
    putStrLn $ show (BSL.length sp) ++ " bytes, " ++ show numbits ++ " bits, " ++ show numones ++ " set"
             ++ " (" ++ Numeric.showFFloat (Just 2) (8*density) "" ++ " bits/byte)"

countBitsValidator :: String -> PlutusTx.CompiledCode a -> IO()
countBitsValidator name val = do
      let erasedValidator = C.deBruijnToIntProgram . C.erasePLCProgram . C.deBruijnPLCProgram $ (PlutusTx.getPlc val)
      putStr $ name ++ ": "
      countBitsProg erasedValidator

printMinSize :: String -> PlutusTx.CompiledCode a -> IO()
printMinSize name val = do
      let erasedValidator = C.deBruijnToIntProgram . C.erasePLCProgram . C.deBruijnPLCProgram $ (PlutusTx.getPlc val)
      putStrLn $ show (BSL.length . compress . serialise $ erasedValidator)

main :: IO()
main = do
  let process = printMinSize
  process    "Crowdfunding"         Crowdfunding.exportedValidator
  process    "Currrency"            Currrency.exportedValidator
  process    "Escrow"               Escrow.exportedValidator
  process    "Future"               Future.exportedValidator
  process    "Game"                 Game.exportedValidator
  process    "GameStateMachine"     GameStateMachine.exportedValidator
  process    "MultiSig"             MultiSig.exportedValidator
  process    "MultiSigStateMachine" MultiSigStateMachine.exportedValidator
  process    "PubKey"               PubKey.exportedValidator
  process    "Swap"                 Swap.exportedValidator
  process    "TokenAccount"         TokenAccount.exportedValidator
  process    "Vesting"              Vesting.exportedValidator

--  let h = merkleHash "rofaoie aewfn9 au"
--      n = U.Prune () h :: U.Term C.IntName ()
--      s = serialise n
--      nb = countBitsBSL s
---  BSL.putStr s
--  let Crowdfunding.exportedValidator
--      val2 = C.deBruijnToIntProgram . C.erasePLCProgram . C.deBruijnPLCProgram $ (PlutusTx.getPlc val)
--  putStrLn . show . prettyPlcClassicDef $ val2
--  BSL.putStr . serialise $ val2
--      v = toFlatTerm . encode $ val2
--  mapM_ (putStrLn . show) v
--      p = toFlatTerm . encode $ val2
--  mapM_ (putStrLn . show) p
--  makeGraph    "Crowdfunding"         Crowdfunding.exportedValidator
--  let program = PlutusTx.getPlc Crowdfunding.exportedValidator
--  putStrLn . show . prettyPlcClassicDef $ program

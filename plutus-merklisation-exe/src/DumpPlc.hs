{- Load the ASTs for the validators of the sample contracts and print
   out a markdown table showing how many of each type of node there
   are, and what percentage of the total number of nodes these make
   up. Also print out a second table showing depths of entire AST and
   depth of nesting of Lams. -}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module DumpPlc (main) where

import           Language.PlutusCore                                   as PLC (Version (..))
import           Language.PlutusCore.Erasure.Untyped.CBOR
--import qualified Language.PlutusCore.Erasure.Untyped.Convert           as C
import qualified Language.PlutusCore.Erasure.Untyped.Term              as U
-- import           Language.PlutusCore.Merkle.Merklisable
-- import qualified Language.PlutusCore.Merkle.Merklise                   as M

import           Language.PlutusTx.Coordination.Contracts.Crowdfunding as Crowdfunding

-- import           Language.PlutusCore.Pretty
import qualified Language.PlutusTx                                     as PlutusTx

import           Codec.Serialise                                       (Serialise, decode, deserialise, encode,
                                                                        serialise)

-- import Language.PlutusCore.Merkle.Merklise                (merklisationStatistics)

-- The use cases import Scripts.hs, which imports Merklise.hs, which
-- imports Untyped.CBOR/CBOR2 This causes overlaps.

import qualified Data.ByteArray                                        as BA
import qualified Data.ByteString.Lazy                                  as BSL

newtype IntName a = IN Int
instance Serialise (IntName ())
b     where encode = undefined
          decode = undefined

testprog :: U.Program IntName ()
testprog = U.Program () (PLC.Version () 1 2 3) (U.Error ())

test :: IO()
test = do
      let u = testprog
      putStrLn $ "\n" ++ show (BSL.length . serialise $ u)

main :: IO()
main = test

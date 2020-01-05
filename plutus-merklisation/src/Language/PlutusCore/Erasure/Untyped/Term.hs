{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.PlutusCore.Erasure.Untyped.Term ( Term (..)
                                , termSubterms
                                , termVars
                                , Value
                                , Program (..)
                                , Constant (..)
                                , Builtin (..)
--                                , BuiltinName (..)
--                                , DynamicBuiltinName (..)
--                                , StagedBuiltinName (..)
                                -- * Helper functions
                                , termLoc
                                -- * Normalized
                                , Normalized (..)
                                , PLC.BuiltinName
                                , PLC.DynamicBuiltinName
                                , PLC.Version
                                ) where

import           Control.Lens             hiding (anon)
import           Crypto.Hash
import qualified Data.ByteString.Lazy     as BSL
import qualified Language.PlutusCore.Core as PLC
import           PlutusPrelude

data Builtin a = BuiltinName a PLC.BuiltinName  -- Just copy Builtin and Constant to simplify things
               | DynBuiltinName a PLC.DynamicBuiltinName
               deriving (Functor, Show, Generic, NFData)


-- | A constant value.
data Constant a = BuiltinInt a Integer
                | BuiltinBS a BSL.ByteString
                | BuiltinStr a String
                deriving (Functor, Show, Generic, NFData)

-- | A 'Term' is a value.
data Term name a = Var a (name a) -- ^ A named variable
                 | LamAbs a (name a) (Term name a)
                 | Apply a (Term name a) (Term name a)
                 | Constant a (Constant a) -- ^ A constant term
                 | Builtin a (Builtin a)
                 | Error a
                 | Prune a (Digest SHA256)
                   deriving (Functor, Show, Generic, NFData)

type Value = Term

data Program name ann = Program ann (PLC.Version ann) (Term name ann)
                        deriving (Show, Functor, Generic, NFData)


termLoc :: Term name a -> a
termLoc (Var l _)      = l
termLoc (Apply l _ _)  = l
termLoc (Constant l _) = l
termLoc (Builtin l _)  = l
termLoc (Error l)      = l
termLoc (LamAbs l _ _) = l
termLoc (Prune l _)    = l

{-# INLINE termSubterms #-}
-- | Get all the direct child 'Term's of the given 'Term'.
termSubterms :: Traversal' (Term name a) (Term name a)
termSubterms f = \case
    LamAbs x n t -> LamAbs x n <$> f t
    Apply x t1 t2 -> Apply x <$> f t1 <*> f t2
    e@Error {} -> pure e
    v@Var {} -> pure v
    c@Constant {} -> pure c
    b@Builtin {} -> pure b
    p@Prune {} -> pure p

-- | Get all the direct child 'name a's of the given 'Term' from 'Var's.
termVars :: Traversal' (Term name a) (name a)
termVars f = \case
    Var a n -> Var a <$> f n
    x -> pure x

newtype Normalized a = Normalized { unNormalized :: a }
    deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
    deriving newtype NFData

instance Applicative Normalized where
    pure = Normalized
    Normalized f <*> Normalized x = Normalized $ f x

instance PrettyBy config a => PrettyBy config (Normalized a) where
    prettyBy config (Normalized x) = prettyBy config x


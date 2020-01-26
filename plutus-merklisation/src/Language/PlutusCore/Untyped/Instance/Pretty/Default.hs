-- | While the flexible pretty-printing infrastructure is useful when you want it,
-- it's helpful to have an implementation of the default Pretty typeclass that
-- does the default thing.

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE UndecidableInstances #-}

module Language.PlutusCore.Untyped.Instance.Pretty.Default () where

import           PlutusPrelude

import           Language.PlutusCore.Pretty.Classic
import           Language.PlutusCore.Untyped.Instance.Pretty.Classic ()
import           Language.PlutusCore.Untyped.Term

instance Pretty (Constant ann) where
    pretty = prettyClassicDef
instance Pretty (Builtin ann) where
    pretty = prettyClassicDef
instance PrettyClassic (name ann) => Pretty (Term name ann) where
    pretty = prettyClassicDef
instance PrettyClassic (name ann) => Pretty (Program name ann) where
    pretty = prettyClassicDef

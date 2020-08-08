{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module Language.PlutusCore.Constant.Function
    ( typeSchemeToType
    , countTermArgs
    , countTypeAndTermArgs
    , dynamicBuiltinNameMeaningToType
    , insertDynamicBuiltinNameDefinition
    , typeOfTypedStaticBuiltinName
    ) where

import           Language.PlutusCore.Constant.Typed
import           Language.PlutusCore.Core
import           Language.PlutusCore.Name

import qualified Data.Map                           as Map
import           Data.Proxy
import qualified Data.Text                          as Text
import           GHC.TypeLits

-- | Convert a 'TypeScheme' to the corresponding 'Type'.
-- Basically, a map from the PHOAS representation to the FOAS one.
typeSchemeToType :: UniOf term ~ uni => TypeScheme term as r -> Type TyName uni ()
typeSchemeToType (TypeSchemeResult pR)           = toTypeAst pR
typeSchemeToType (TypeSchemeArrow pA schB)       = TyFun () (toTypeAst pA) $ typeSchemeToType schB
typeSchemeToType (TypeSchemeAll proxy kind schK) = case proxy of
    (_ :: Proxy '(text, uniq)) ->
        let text = Text.pack $ symbolVal @text Proxy
            uniq = fromIntegral $ natVal @uniq Proxy
            a    = TyName $ Name text $ Unique uniq
        in TyForall () a kind $ typeSchemeToType (schK Proxy)

countTermArgs :: TypeScheme uni as r -> Int
countTermArgs (TypeSchemeResult _)     = 0
countTermArgs (TypeSchemeArrow _ schB) = 1 + countTermArgs schB
countTermArgs (TypeSchemeAll _ _ schK) = countTermArgs (schK Proxy)

countTypeAndTermArgs :: TypeScheme uni as r -> Int
countTypeAndTermArgs (TypeSchemeResult _)     = 0
countTypeAndTermArgs (TypeSchemeArrow _ schB) = 1 + countTypeAndTermArgs schB
countTypeAndTermArgs (TypeSchemeAll _ _ schK) = 1 + countTypeAndTermArgs (schK Proxy)

-- | Extract the 'TypeScheme' from a 'DynamicBuiltinNameMeaning' and
-- convert it to the corresponding 'Type'.
dynamicBuiltinNameMeaningToType
    :: UniOf term ~ uni => DynamicBuiltinNameMeaning term -> Type TyName uni ()
dynamicBuiltinNameMeaningToType (DynamicBuiltinNameMeaning sch _ _) = typeSchemeToType sch

-- | Insert a 'DynamicBuiltinNameDefinition' into a 'DynamicBuiltinNameMeanings'.
insertDynamicBuiltinNameDefinition
    :: DynamicBuiltinNameDefinition uni
    -> DynamicBuiltinNameMeanings uni
    -> DynamicBuiltinNameMeanings uni
insertDynamicBuiltinNameDefinition
    (DynamicBuiltinNameDefinition name mean) (DynamicBuiltinNameMeanings nameMeans) =
        DynamicBuiltinNameMeanings $ Map.insert name mean nameMeans

-- | Return the 'Type' of a 'TypedBuiltinName'.
typeOfTypedStaticBuiltinName
    :: UniOf term ~ uni => TypedStaticBuiltinName term as r -> Type TyName uni ()
typeOfTypedStaticBuiltinName (TypedStaticBuiltinName _ scheme) = typeSchemeToType scheme

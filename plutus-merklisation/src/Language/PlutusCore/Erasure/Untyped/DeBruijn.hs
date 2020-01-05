-- Stolen from Language.PlutusCore.DeBruijn


{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Support for using de Bruijn indices for term and type names.
module Language.PlutusCore.Erasure.Untyped.DeBruijn
    ( Index (..)
    , DeBruijn (..)
    , FreeVariableError (..)
    , deBruijnTerm
    , deBruijnProgram
    , unDeBruijnTerm
    , unDeBruijnProgram
    ) where

import           Control.Exception
import           Control.Lens                             hiding (Index, Level, index, ix)
import           Control.Monad.Except
import           Control.Monad.Reader

import           Language.PlutusCore.DeBruijn             (DeBruijn (..), Index (..))
import           Language.PlutusCore.Erasure.Untyped.Term
import           Language.PlutusCore.Name
import           Language.PlutusCore.Quote

import qualified Data.Bimap                               as BM
import           Data.Typeable


class HasIndex a where
    index :: Lens' a Index

instance HasIndex (DeBruijn ann) where
    index = lens g s where
        g = dbnIndex
        s n i = n{dbnIndex=i}


-- Converting from normal names to DeBruijn indices, and vice versa

{- Note [Levels and indices]
The indices ('Index') that we actually store as our de Bruijn indices in the program
are *relative* - that is, they say how many levels above the *current* level to look for
the binder.

However, when doing conversions it is easier to record the  *absolute* level of a variable,
in our state, since that way we don't have to adjust our mapping when we go under a binder (whereas
for relative indices we would need to increment them all by one, as the current level has increased).

However, this means that we *do* need to do an adjustment when we store an index as a level or extract
a level to use it as an index. The adjustment is fairly straightforward:
- An index `i` points to a binder `i` levels above (smaller than) the current level, so the level
  of `i` is `current - i`.
- A level `l` which is `i` levels above (smaller than) the current level has an index of `i`, so it
  is also calculated as `current - l`.

We use a newtype to keep these separate, since getting it wrong will leads to annoying bugs.
-}

-- | An absolute level in the program.
newtype Level = Level Index deriving newtype (Eq, Ord, Num)
data Levels = Levels Level (BM.Bimap Unique Level)

-- | Compute the absolute 'Level' of a relative 'Index' relative to the current 'Level'.
ixToLevel :: Level -> Index -> Level
ixToLevel (Level current) ix = Level (current - ix)

-- | Compute the relative 'Index' of a absolute 'Level' relative to the current 'Level'.
levelToIndex :: Level -> Level -> Index
levelToIndex (Level current) (Level l) = current - l

-- | Declare a name with a unique, recording the mapping to a 'Level'.
declareUnique :: (MonadReader Levels m, HasUnique name unique) => name -> m a -> m a
declareUnique n =
    local $ \(Levels current ls) -> Levels current $ BM.insert (n ^. theUnique) current ls

-- | Declare a name with an index, recording the mapping from the corresponding 'Level' to a fresh unique.
declareIndex :: (MonadReader Levels m, MonadQuote m, HasIndex name) => name -> m a -> m a
declareIndex n act = do
    newU <- freshUnique
    local (\(Levels current ls) -> Levels current $ BM.insert newU (ixToLevel current (n ^. index)) ls) act

-- | Enter a scope, incrementing the current 'Level' by one
withScope :: MonadReader Levels m => m a -> m a
withScope = local $ \(Levels current ls) -> Levels (current+1) ls

-- | We cannot do a correct translation to or from de Bruijn indices if the program is not well-scoped.
-- So we throw an error in such a case.
data FreeVariableError
    = FreeUnique Unique
    | FreeIndex Index
    deriving (Show, Typeable, Eq, Ord)
instance Exception FreeVariableError

-- | Get the 'Index' corresponding to a given 'Unique'.
getIndex :: (MonadReader Levels m, MonadError FreeVariableError m) => Unique -> m Index
getIndex u = do
    Levels current ls <- ask
    case BM.lookup u ls of
        Just ix -> pure $ levelToIndex current ix
        Nothing -> throwError $ FreeUnique u

-- | Get the 'Unique' corresponding to a given 'Index'.
getUnique :: (MonadReader Levels m, MonadError FreeVariableError m) => Index -> m Unique
getUnique ix = do
    Levels current ls <- ask
    case BM.lookupR (ixToLevel current ix) ls of
        Just u  -> pure u
        Nothing -> throwError $ FreeIndex ix

nameToDeBruijn
    :: (MonadReader Levels m, MonadError FreeVariableError m)
    => Name ann -> m (DeBruijn ann)
nameToDeBruijn (Name ann str u) = DeBruijn ann str <$> getIndex u

deBruijnToName
    :: (MonadReader Levels m, MonadError FreeVariableError m)
    => DeBruijn ann -> m (Name ann)
deBruijnToName (DeBruijn ann str ix) = Name ann str <$> getUnique ix



-- | Convert a 'Term' with 'TyName's and 'Name's into a 'Term' with 'TyDeBruijn's and 'DeBruijn's.
deBruijnTerm
    :: MonadError FreeVariableError m
    => Term Name ann -> m (Term DeBruijn ann)
deBruijnTerm = flip runReaderT (Levels 0 BM.empty) . deBruijnTermM

-- | Convert a 'Program' with 'TyName's and 'Name's into a 'Program' with 'TyDeBruijn's and 'DeBruijn's.
deBruijnProgram
    :: MonadError FreeVariableError m
    => Program Name ann -> m (Program DeBruijn ann)
deBruijnProgram (Program ann ver term) = Program ann ver <$> deBruijnTerm term


deBruijnTermM
    :: (MonadReader Levels m, MonadError FreeVariableError m)
    => Term Name ann
    -> m (Term DeBruijn ann)
deBruijnTermM = \case
    -- variable case
    Var ann n -> Var ann <$> nameToDeBruijn n
    -- binder cases
    LamAbs ann n t -> declareUnique n $ do
        n' <- nameToDeBruijn n
        withScope $ LamAbs ann n' <$> deBruijnTermM t
    -- boring recursive cases
    Apply ann t1 t2 -> Apply ann <$> deBruijnTermM t1 <*> deBruijnTermM t2
    -- boring non-recursive cases
    Error ann        -> pure $ Error ann
    Constant ann con -> pure $ Constant ann con
    Builtin ann bn   -> pure $ Builtin ann bn
    Prune ann h      -> pure $ Prune ann h

-- | Convert a 'Term' with 'TyDeBruijn's and 'DeBruijn's into a 'Term' with 'TyName's and 'Name's.
unDeBruijnTerm
    :: (MonadQuote m, MonadError FreeVariableError m)
    => Term DeBruijn ann -> m (Term Name ann)
unDeBruijnTerm = flip runReaderT (Levels 0 BM.empty) . unDeBruijnTermM

-- | Convert a 'Program' with 'TyDeBruijn's and 'DeBruijn's into a 'Program' with 'TyName's and 'Name's.
unDeBruijnProgram
    :: (MonadQuote m, MonadError FreeVariableError m)
    => Program DeBruijn ann -> m (Program Name ann)
unDeBruijnProgram (Program ann ver term) = Program ann ver <$> unDeBruijnTerm term


unDeBruijnTermM
    :: (MonadReader Levels m, MonadQuote m, MonadError FreeVariableError m)
    => Term DeBruijn ann
    -> m (Term Name ann)
unDeBruijnTermM = \case
    -- variable case
    Var ann n -> Var ann <$> deBruijnToName n
    -- binder cases
    LamAbs ann n t -> declareIndex n $ do
        n' <- deBruijnToName n
        withScope $ LamAbs ann n' <$> unDeBruijnTermM t
    -- boring recursive cases
    Apply ann t1 t2 -> Apply ann <$> unDeBruijnTermM t1 <*> unDeBruijnTermM t2
    -- boring non-recursive cases
    Error ann        -> pure $ Error ann
    Constant ann con -> pure $ Constant ann con
    Builtin ann bn   -> pure $ Builtin ann bn
    Prune ann h      -> pure $ Prune ann h

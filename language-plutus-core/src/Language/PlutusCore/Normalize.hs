-- | The user-facing API of the normalizer.

module Language.PlutusCore.Normalize
    ( normalizeType
    , normalizeTypeFull
    , normalizeTypeMana
    , normalizeTypesIn
    , normalizeTypesFullIn
    , normalizeTypesManaIn
    , normalizeTypesFullInProgram
    ) where

import           Language.PlutusCore.Name
import           Language.PlutusCore.Normalize.Internal
import           Language.PlutusCore.Quote
import           Language.PlutusCore.Rename
import           Language.PlutusCore.Type

import           Control.Monad                          ((>=>))

-- See Note [Normalization].
-- | Normalize a 'Type'.
normalizeType
    :: (HasUnique (tyname ann) TypeUnique, MonadQuote m)
    => m () -> Type tyname ann -> m (Normalized (Type tyname ann))
normalizeType countStep = rename >=> runNormalizeTypeM countStep . normalizeTypeM

-- See Note [Normalization].
-- | Normalize a 'Type' without dealing with mana.
normalizeTypeFull
    :: (HasUnique (tyname ann) TypeUnique, MonadQuote m)
    => Type tyname ann -> m (Normalized (Type tyname ann))
normalizeTypeFull = normalizeType $ pure ()

-- | Normalize a 'Type' in the mana-consuming way.
-- Count a single substitution step by subtracting @1@ from available mana or
-- fail when there is no available mana.
normalizeTypeMana
    :: (HasUnique (tyname ann) TypeUnique, MonadQuote m)
    => Mana -> Type tyname ann -> m (Maybe (Normalized (Type tyname ann)))
normalizeTypeMana mana = rename >=> runNormalizeTypeManaM mana . normalizeTypeM

-- | Normalize every 'Type' in a 'Term'.
normalizeTypesIn
    :: (HasUnique (tyname ann) TypeUnique, HasUnique (name ann) TermUnique, MonadQuote m)
    => m () -> Term tyname name ann -> m (Term tyname name ann)
normalizeTypesIn countStep = rename >=> runNormalizeTypeM countStep . normalizeTypesInM

-- | Normalize every 'Type' in a 'Term' without dealing with mana.
normalizeTypesFullIn
    :: (HasUnique (tyname ann) TypeUnique, HasUnique (name ann) TermUnique, MonadQuote m)
    => Term tyname name ann -> m (Term tyname name ann)
normalizeTypesFullIn = normalizeTypesIn $ pure ()

-- | Normalize every 'Type' in a 'Term' in the mana-consuming way.
-- Count a single substitution step by subtracting @1@ from available mana or
-- fail when there is no available mana.
normalizeTypesManaIn
    :: (HasUnique (tyname ann) TypeUnique, HasUnique (name ann) TermUnique, MonadQuote m)
    => Mana -> Term tyname name ann -> m (Maybe (Term tyname name ann))
normalizeTypesManaIn mana = rename >=> runNormalizeTypeManaM mana . normalizeTypesInM

-- | Normalize every 'Type' in a 'Program' without dealing with mana.
normalizeTypesFullInProgram
    :: (HasUnique (tyname ann) TypeUnique, HasUnique (name ann) TermUnique, MonadQuote m)
    => Program tyname name ann -> m (Program tyname name ann)
normalizeTypesFullInProgram (Program x v t) = Program x v <$> normalizeTypesFullIn t

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeApplications   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Orphans where

import           Crypto.Hash                (Digest, SHA256)
import           IOTS                       (HasReps (typeReps))
import qualified Language.PlutusTx.AssocMap as Map
import qualified Language.PlutusTx.Prelude  as P
import           Schema                     (ToSchema, toSchema)
import           Type.Reflection            (Typeable)

instance ToSchema (Digest SHA256) where
  toSchema = toSchema @String

instance ToSchema P.ByteString where
  toSchema = toSchema @String

instance HasReps (Digest SHA256) where
  typeReps = typeReps @String

instance HasReps P.ByteString where
  typeReps = typeReps @String

instance (Typeable k, Typeable v) => ToSchema (Map.Map k v)

instance (Typeable k, Typeable v, HasReps k, HasReps v) =>
         HasReps (Map.Map k v)

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Metadata.Types where

import           Control.Applicative       (Alternative, (<|>))
import           Control.Lens              (makeLenses)
import           Control.Monad.Freer.TH    (makeEffect)
import           Control.Newtype.Generics  (Newtype)
import qualified Control.Newtype.Generics  as Newtype
import           Data.Aeson                (FromJSON, ToJSON, parseJSON, toJSON, withObject, withText, (.:))
import qualified Data.Aeson                as JSON
import           Data.Aeson.Extras         (decodeByteString, encodeByteString)
import           Data.Aeson.Types          (Parser)
import qualified Data.ByteString           as BS
import           Data.List.NonEmpty        (NonEmpty)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc (Pretty, pretty, viaShow, (<+>))
import           GHC.Generics              (Generic)
import           Ledger.Crypto             (PubKey (PubKey), PubKeyHash, Signature (Signature), getPubKey,
                                            getPubKeyHash)
import           LedgerBytes               (LedgerBytes)
import qualified LedgerBytes
import           Servant.API               (FromHttpApiData, ToHttpApiData)
import           Servant.Client            (BaseUrl)

newtype MetadataConfig =
    MetadataConfig
        { mdBaseUrl :: BaseUrl
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON)

------------------------------------------------------------
newtype Subject =
    Subject Text
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, Pretty)

class ToSubject a where
    toSubject :: a -> Subject

instance ToSubject BS.ByteString where
    toSubject x = Subject $ encodeByteString x

instance ToSubject LedgerBytes where
    toSubject = toSubject . LedgerBytes.bytes

instance ToSubject PubKey where
    toSubject = toSubject . getPubKey

instance ToSubject PubKeyHash where
    toSubject = toSubject . getPubKeyHash

------------------------------------------------------------
newtype PropertyKey =
    PropertyKey Text
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, Pretty)

newtype PropertyName =
    PropertyName Text
    deriving (Show, Eq, Generic)
    deriving newtype (ToJSON, FromJSON, Pretty)

data Property =
    Property
        { _propertySubject     :: Subject
        , _propertyDescription :: PropertyDescription
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype LivePropertyDescription =
    LivePropertyDescription PropertyDescription
    deriving (Eq, Show, Generic)
    deriving anyclass (Newtype)

instance FromJSON LivePropertyDescription where
    parseJSON =
        withObject "PropertyDescription" $ \o ->
            LivePropertyDescription <$>
            (parseName o <|> parseDescription o <|> parsePreimage o <|>
             parseOwner o)

data LivePropertyDescriptions =
    LivePropertyDescriptions Subject [PropertyDescription]

instance FromJSON LivePropertyDescriptions where
    parseJSON =
        withObject "PropertyDescriptions" $ \o -> do
            subject <- o .: "subject"
            LivePropertyDescriptions subject <$>
                accumulateSuccesses
                    [ parseName o
                    , parseDescription o
                    , parsePreimage o
                    , parseOwner o
                    ]

accumulateSuccesses ::
       ( Alternative f
       , Applicative m
       , Monoid (m a)
       , Monoid (f (m a))
       , Foldable g
       )
    => g (f a)
    -> f (m a)
accumulateSuccesses = foldMap (\parser -> (pure <$> parser) <|> pure mempty)

parseName :: JSON.Object -> Parser PropertyDescription
parseName =
    subParser
        "name"
        "name"
        (\subObject -> do
             value <- subObject .: "value"
             signatures :: NonEmpty LiveAnnotatedSignature <-
                 subObject .: "anSignatures"
             pure $ Name value (Newtype.unpack <$> signatures))

parseDescription :: JSON.Object -> Parser PropertyDescription
parseDescription =
    subParser
        "description"
        "description"
        (\description -> do
             value <- description .: "value"
             signatures :: NonEmpty LiveAnnotatedSignature <-
                 description .: "anSignatures"
             pure $ Description value (Newtype.unpack <$> signatures))

parsePreimage :: JSON.Object -> Parser PropertyDescription
parsePreimage =
    subParser
        "preImage"
        "preImage"
        (\image -> do
             hash <- image .: "hashFn"
             hex <- image .: "hex"
             pure $ Preimage hash hex)

parseOwner :: JSON.Object -> Parser PropertyDescription
parseOwner subObject = do
    sig :: LiveAnnotatedSignature <- subObject .: "owner"
    pure $ Other "owner" (JSON.Object subObject) (pure (Newtype.unpack sig))

subParser ::
       Text -> String -> (JSON.Object -> Parser a) -> JSON.Object -> Parser a
subParser key name parser o = do
    subValue <- o .: key
    withObject name parser subValue

instance Pretty Property where
    pretty = viaShow

data HashFunction
    = SHA256
    | Blake2B256
    deriving (Show, Eq, Ord, Generic, Enum, Bounded)

instance FromJSON HashFunction where
    parseJSON =
        withText "HashFunction" $ \case
            "SHA256" -> pure SHA256
            "blake2b-256" -> pure Blake2B256
            other -> fail $ "Unknown HashFunction '" <> Text.unpack other <> "'"

instance ToJSON HashFunction where
    toJSON SHA256     = JSON.String "SHA256"
    toJSON Blake2B256 = JSON.String "blake2b-256"

data PropertyDescription
    = Preimage HashFunction LedgerBytes
    | Name Text (NonEmpty AnnotatedSignature)
    | Description Text (NonEmpty AnnotatedSignature)
    | Other Text JSON.Value (NonEmpty AnnotatedSignature)
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data AnnotatedSignature =
    AnnotatedSignature PubKey Signature
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype LiveAnnotatedSignature =
    LiveAnnotatedSignature AnnotatedSignature
    deriving (Eq, Show, Generic)
    deriving anyclass (Newtype)

instance FromJSON LiveAnnotatedSignature where
    parseJSON =
        withObject "AnnotatedSignature" $ \o -> do
            pubKeyRaw <- o .: "publicKey"
            sigRaw <- o .: "signature"
            sigBytes <- decodeByteString sigRaw
            let pubKey = PubKey $ LedgerBytes.fromHex pubKeyRaw
                sig = Signature sigBytes
            pure $ LiveAnnotatedSignature $ AnnotatedSignature pubKey sig

makeLenses ''Property

toId :: Property -> (Subject, PropertyKey)
toId (Property subject description) = (subject, toPropertyKey description)

toPropertyKey :: PropertyDescription -> PropertyKey
toPropertyKey (Preimage _ _)    = PropertyKey "preimage"
toPropertyKey (Name _ _)        = PropertyKey "name"
toPropertyKey (Description _ _) = PropertyKey "description"
toPropertyKey (Other name _ _)  = PropertyKey name

toPropertyList :: LivePropertyDescriptions -> [Property]
toPropertyList (LivePropertyDescriptions subject descriptions) =
    Property subject <$> descriptions

------------------------------------------------------------
data MetadataEffect r where
    GetProperties :: Subject -> MetadataEffect [Property]
    GetProperty :: Subject -> PropertyKey -> MetadataEffect Property

makeEffect ''MetadataEffect

------------------------------------------------------------

data MetadataError
    = SubjectNotFound Subject
    | PropertyNotFound Subject PropertyKey
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data MetadataLogMessage
    = FetchingSubject Subject
    | FetchingProperty Subject PropertyKey

instance Pretty MetadataLogMessage where
    pretty =
        \case
            FetchingSubject subject -> "Fetching subject:" <+> pretty subject
            FetchingProperty subject propertyKey ->
                "Fetching property:" <+>
                pretty subject <> "/" <> pretty propertyKey

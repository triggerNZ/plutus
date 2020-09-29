{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Metadata.TypesSpec
    ( tests
    ) where

import           Cardano.Metadata.Client   (handleMetadataClient)
import           Cardano.Metadata.Types    (AnnotatedSignature (AnnotatedSignature), HashFunction, MetadataError,
                                            PropertyDescription (Name), Subject (Subject),
                                            SubjectProperties (SubjectProperties), getProperties)
import           Control.Monad             (void)
import           Control.Monad.Freer       (runM)
import           Control.Monad.Freer.Error (runError)
import           Data.Aeson                (FromJSON, eitherDecode, encode)
import           Data.Aeson.Extras         (tryDecode)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Either.Extras        (unsafeFromEither)
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import           Data.Text                 (Text)
import           Ledger.Crypto             (Signature (Signature))
import           Network.HTTP.Client       (managerModifyRequest, newManager, setRequestIgnoreStatus)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Servant.Client            (mkClientEnv, parseBaseUrl)
import qualified Test.SmallCheck.Series    as SC
import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.HUnit          (HasCallStack, assertEqual, assertFailure, testCase)
import qualified Test.Tasty.SmallCheck     as SC

tests :: TestTree
tests = testGroup "Cardano.Metadata.Types" [jsonTests]

jsonTests :: TestTree
jsonTests =
    testGroup
        "JSON Encoding"
        [ testGroup
              "JSON Encoding"
              [ SC.testProperty "Roundtrip encoding of HashFunction" $ \(h :: HashFunction) ->
                    Right h == eitherDecode (encode h)
              ]
        , testGroup
              "Metadata Server Responses FromJSON"
              [ testCase "Signature response" $
                void $
                assertDecodes
                    @(NonEmpty AnnotatedSignature)
                    "test/Cardano/Metadata/signature_response1.json"
              , testGroup
                    "Property query response"
                    [ testCase "owner" $
                      void $
                      assertDecodes
                          @PropertyDescription
                          "test/Cardano/Metadata/property_owner.json"
                    , testCase "name" $
                      void $
                      assertDecodes
                          @PropertyDescription
                          "test/Cardano/Metadata/property_name.json"
                    , testCase "preImage" $
                      void $
                      assertDecodes
                          @PropertyDescription
                          "test/Cardano/Metadata/property_preimage.json"
                    , testCase "description" $
                      void $
                      assertDecodes
                          @PropertyDescription
                          "test/Cardano/Metadata/property_description.json"
                    ]
              , testCase "Subject query response" $
                void $
                assertDecodes
                    @SubjectProperties
                    "test/Cardano/Metadata/subject_response1.json"
              ]
        , testCase "World" $ do
              mdBaseUrl <- parseBaseUrl "https://api.cardano.org/staging/"
              manager <- newManager $ tlsManagerSettings {managerModifyRequest = pure . setRequestIgnoreStatus}
              let clientEnv = mkClientEnv manager mdBaseUrl
              result :: Either MetadataError SubjectProperties <-
                  runM .
                  runError . handleMetadataClient clientEnv . getProperties $
                  Subject
                      "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"
              assertEqual
                  "Results"
                  (Right
                       (SubjectProperties
                            (Subject
                                 "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0")
                            [ Name
                                  "SteveToken"
                                  (AnnotatedSignature
                                       "0ee262f062528667964782777917cd7139e19e8eb2c591767629e4200070c661"
                                       (toSignature
                                            "7ef6ed44ba9456737ef8d2e31596fdafb66d5775ac1a254086a553b666516e5895bb0c6b7ba8bef1f6b4d9bd9253b4449d1354de2f9e043ea4eb43fd42f87108") :|
                                   [ AnnotatedSignature
                                         "7c3bfe2a11290a9b6ea054b4d0932678f88130511cfbfe3f634ee77d71edebe7"
                                         (toSignature
                                              "c95cf87b74d1e4d3b413c927c65de836f0905ba2cd176c7cbff83d8b886b30fe1560c542c1f77bb88280dff55c2d267c9840fe36560fb13ba4a78b6429e51500")
                                   , AnnotatedSignature
                                         "8899d0777f399fffd44f72c85a8aa51605123a7ebf20bba42650780a0c81096a"
                                         (toSignature
                                              "f88692b13212bac8121151a99a4de4d5244e5f63566babd2b8ac20950ede74073af0570772b3ce3d11b72e972079199f02306e947cd5fcca688a9d4664eddb04")
                                   , AnnotatedSignature
                                         "d40688a3eeda1f229c64efc56dd53b363ff981f71a7462f78c8cc444117a03db"
                                         (toSignature
                                              "c2b30fa5f2c09323d81e5050af681c023089d832d0b85d05f60f4278fba3011ab03e6bd9bd2b8649080a368ecfe51573cd232efe8f1e7ca69ff8334ced7b6801")
                                   ])
                            ]))
                  result
        ]

toSignature :: Text -> Signature
toSignature = Signature . unsafeFromEither . tryDecode

assertDecodes ::
       forall a. (FromJSON a, HasCallStack)
    => FilePath
    -> IO a
assertDecodes filename = do
    rawJSON <- LBS.readFile filename
    case eitherDecode rawJSON of
        Left err    -> assertFailure err
        Right value -> pure value

-- | To be able to smallcheck HashFunction.
instance SC.Serial IO HashFunction where
    series = SC.generate (const [minBound .. maxBound])

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Metadata.TypesSpec
    ( tests
    ) where

import           Cardano.Metadata.Types (HashFunction, LiveAnnotatedSignature, LivePropertyDescription,
                                         LivePropertyDescriptions)
import           Control.Monad          (void)
import           Data.Aeson             (FromJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy   as LBS
import           Data.List.NonEmpty     (NonEmpty)
import qualified Test.SmallCheck.Series as SC
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (assertFailure, testCase)
import qualified Test.Tasty.SmallCheck  as SC

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
                    @(NonEmpty LiveAnnotatedSignature)
                    "test/Cardano/Metadata/signature_response1.json"
              , testGroup
                    "Property query response"
                    [ testCase "owner" $
                      void $
                      assertDecodes
                          @LivePropertyDescription
                          "test/Cardano/Metadata/property_owner.json"
                    , testCase "name" $
                      void $
                      assertDecodes
                          @LivePropertyDescription
                          "test/Cardano/Metadata/property_name.json"
                    , testCase "preImage" $
                      void $
                      assertDecodes
                          @LivePropertyDescription
                          "test/Cardano/Metadata/property_preimage.json"
                    , testCase "description" $
                      void $
                      assertDecodes
                          @LivePropertyDescription
                          "test/Cardano/Metadata/property_description.json"
                    ]
              , testCase "Subject query response" $
                void $
                assertDecodes
                    @LivePropertyDescriptions
                    "test/Cardano/Metadata/subject_response1.json"
              ]
        ]

instance SC.Serial IO HashFunction where
  series = SC.generate (const [minBound .. maxBound])

assertDecodes ::
       forall a. FromJSON a
    => FilePath
    -> IO a
assertDecodes filename = do
    rawJSON <- LBS.readFile filename
    case eitherDecode rawJSON of
        Left err    -> assertFailure err
        Right value -> pure value

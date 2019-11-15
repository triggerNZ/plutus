module Simulation.AccountIdEditor where

import Prelude hiding (div, min)
import Bootstrap (col, colFormLabel, col_, formControl, formGroup, formRow_)
import Data.BigInteger as BigInteger
import Halogen.HTML (HTML, div, input, label, text)
import Halogen.HTML.Events (onValueInput)
import Halogen.HTML.Properties (InputType(..), classes, min, placeholder, required, type_, value)
import Marlowe.Semantics (AccountId(..))
import Text.Parsing.Parser.Pos (Position)
import Types (HAction(..))

accountIdEditor :: forall p. String -> AccountId -> Position -> Position -> HTML p HAction
accountIdEditor fieldName accountId start end =
  div []
    [ fieldNameRow fieldName accountId
    , accountNumberRow fieldName accountId
    , accountNameRow fieldName accountId
    ]

accountNumberRow :: forall p. String -> AccountId -> HTML p HAction
accountNumberRow fieldName (AccountId accNumber accName) =
  div
    [ classes
        [ formGroup
        ]
    ]
    [ formRow_
        $ [ label
              [ classes [ col, colFormLabel ] ]
              [ text fieldName
              ]
          , col_
              [ input
                  [ type_ InputNumber
                  , classes [ formControl ]
                  , value $ show accNumber
                  , required true
                  , placeholder "Account Number"
                  , min zero
                  , onValueInput
                      $ \str -> do
                          newAccNumber <- BigInteger.fromString str
                          pure $ SetAccountId fieldName $ AccountId newAccNumber accName
                  ]
              ]
          ]
    ]

accountNameRow :: forall p. String -> AccountId -> HTML p HAction
accountNameRow fieldName (AccountId accNumber accName) =
  div
    [ classes
        [ formGroup
        ]
    ]
    [ formRow_
        $ [ label
              [ classes [ col, colFormLabel ] ]
              [ text fieldName
              ]
          , col_
              [ input
                  [ type_ InputText
                  , classes [ formControl ]
                  , value accName
                  , required true
                  , placeholder "Account Name"
                  , min zero
                  , onValueInput $ \str -> pure $ SetAccountId fieldName $ AccountId accNumber str
                  ]
              ]
          ]
    ]

fieldNameRow :: forall p. String -> AccountId -> HTML p HAction
fieldNameRow fieldName (AccountId accNumber accName) =
  div
    [ classes
        [ formGroup
        ]
    ]
    [ formRow_
        $ [ label
              [ classes [ col, colFormLabel ] ]
              [ text fieldName
              ]
          , col_
              [ input
                  [ type_ InputText
                  , classes [ formControl ]
                  , value fieldName
                  , required true
                  , placeholder "Variable Name"
                  , min zero
                  , onValueInput $ \str -> pure $ SetAccountId str $ AccountId accNumber accName
                  ]
              ]
          ]
    ]

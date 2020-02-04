{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

-- import qualified Spec.Actus
import qualified Spec.Marlowe.Marlowe

import           Test.Tasty
import           Data.Either
import           Data.Text
import           Test.Tasty.Hedgehog  (HedgehogTestLimit (..))
import           Test.Tasty.HUnit
import Text.Megaparsec.Error
import Language.Marlex
import Language.Marlowe hiding (Let)
import qualified Language.Marlowe as Marlowe

main :: IO ()
main = defaultMain tests

-- | Number of successful tests for each hedgehog property.
--   The default is 100 but we use a smaller number here in order to speed up
--   the test suite.
--
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 3)

tests :: TestTree
tests = localOption limit $ testGroup "Marlowe Contracts"
        [
            -- Spec.Marlowe.Marlowe.tests
            testCase "parse" parseTests
        ]

parseTests = do
    let p :: Text -> Expr
        p t = case parseExpr t of
                Left e -> error (errorBundlePretty e)
                Right r -> r
        convert t = case convertContract (eval defaultEnv (p t)) of
            Left e -> error e
            Right r -> r
    p "True" @?= Ident "True"
    p "\"string\"" @?= Lit (S "string")
    p "{-11-}123--asdf" @?= Lit (I 123)
    p "a (b c) d" @?= App (Ident "a") [App (Ident "b") [Ident "c"], Ident "d"]
    p "\\a b -> a1 23" @?= Lam ["a", "b"] (App (Ident "a1") [Lit $ I 23])
    p "leti" @?= Ident "leti"
    p "let {a=5; b = letin} in letin a b" @?=
        Let [Bind "a" (Lit (I 5)), Bind "b" (Ident "letin")]
            (App (Ident "letin") [Ident "a", Ident "b"])
    convert "Close" @?= Close
    p "When a 42 Close" @?= App (Ident "When") [Ident "a",Lit (I 42),Ident "Close"]
    convert "When Close (Slot 42) Close" @?= When [] 42 Close

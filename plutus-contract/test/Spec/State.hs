{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Spec.State where

import           Control.Monad.Freer                (Eff, run)
import           Control.Monad.Freer.Extras         (raiseEnd)
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust)
import           Test.Tasty
import qualified Test.Tasty.HUnit                   as HUnit

import           Language.Plutus.Contract.Resumable (IterationID (..), Record (..), Request (..), RequestID (..),
                                                     RequestState (..), Resumable, prompt, select)
import qualified Language.Plutus.Contract.Resumable as S
import           Language.Plutus.Contract.Util      (loopM)

runResumableTest ::
    forall i o a.
    Record i
    -> Eff '[Resumable i o] a
    -> (Maybe a, RequestState o)
runResumableTest record =
    run
    . runState S.initialRequestState
    . runReader record
    . S.handleNonDetPrompt @i @o @a
    . S.handleResumable
    . raiseEnd

tests :: TestTree
tests = testGroup "stateful contract"
    [ HUnit.testCase "run a contract without prompts" $
        let res = runResumableTest @Int @String mempty (pure ())
        in HUnit.assertBool "run a contract without prompts" (isJust $ fst res)

    , HUnit.testCase "run a contract with a single prompt" $
        let (_, RequestState{rsOpenRequests}) = runResumableTest @Int @String mempty (askStr "prompt1")
        in HUnit.assertEqual
            "run a contract with a single prompt"
            rsOpenRequests
            [Request{rqID = RequestID 1, itID = IterationID 1, rqRequest = "prompt1"}]

    , HUnit.testCase "run a contract with two prompts" $
        let (_, RequestState{rsOpenRequests}) = runResumableTest @Int @String mempty (askStr "prompt1" `selectStr` askStr "prompt2")
        in HUnit.assertEqual
            "run a contract with two prompts"
            [ Request{rqID = RequestID 2, itID = IterationID 1, rqRequest = "prompt2"}
            , Request{rqID = RequestID 1, itID = IterationID 1, rqRequest = "prompt1"}
            ]
            rsOpenRequests

    , HUnit.testCase "run a contract with a two prompts and one answer" $
        let record = Record $ Map.singleton (IterationID 1) $ Map.singleton (RequestID 2) 5
            (result, _) = runResumableTest @Int @String record ((askStr "prompt1" >> pure "branch 1") `selectStr` (askStr "prompt2" >> pure "branch 2"))
        in HUnit.assertEqual "run a contract with a two prompts and one answer" (Just "branch 2") result

    , HUnit.testCase "commit to a branch" $
        let record = Record $ Map.singleton (IterationID 1) $ Map.singleton (RequestID 1) 5
            (_, RequestState{rsOpenRequests}) = runResumableTest @Int @String record ((askStr "prompt1" >> askStr "prompt3") `selectStr` (askStr "prompt2" >> pure 10))
        in HUnit.assertEqual
                "commit to a branch"
                [ Request{rqID = RequestID 2, itID = IterationID 2, rqRequest = "prompt3"} ]
                rsOpenRequests

    , HUnit.testCase "commit to a branch (II)" $
        let record = Record $ Map.singleton (IterationID 1) $ Map.singleton (RequestID 2) 5
            (_, RequestState{rsOpenRequests}) = runResumableTest @Int @String record ((askStr "prompt2" >> pure 10) `selectStr` (askStr "prompt1" >> askStr "prompt3"))
        in HUnit.assertEqual
            "commit to a branch (II)"
            [ Request{rqID = RequestID 3, itID = IterationID 2, rqRequest = "prompt3"} ]
            rsOpenRequests

    , HUnit.testCase "return a result" $
        let record = Record $ Map.singleton (IterationID 1) $ Map.singleton (RequestID 2) 5
            (result, _) = runResumableTest @Int @String record ((askStr "prompt1" >> askStr "prompt4") `selectStr` (askStr "prompt2" >> pure 10) `selectStr` (askStr "prompt3" >> askStr "prompt5"))
        in HUnit.assertEqual "return a result" (Just 10) result

    , HUnit.testCase "go into a branch" $
        let record = Record $ Map.fromList [(IterationID 1, Map.singleton (RequestID 2) 5), (IterationID 2, Map.singleton (RequestID 4) 10)]
            (result, _) = runResumableTest @Int @String record
                ((askStr "prompt1" >> askStr "prompt4")
                `selectStr`
                    (askStr "prompt2" >> (askStr "prompt5" `selectStr` (askStr "prompt6" >> pure 11) `selectStr` askStr "prompt8"))
                    `selectStr` (askStr "prompt3" >> askStr "prompt7"))
        in HUnit.assertEqual "go into a branch" (Just 11) result

    , HUnit.testCase "loop" $
        let record = Record
                 $ Map.fromList
                    [ (IterationID 1, Map.singleton (RequestID 1) 1)
                    , (IterationID 2, Map.singleton (RequestID 2) 1)
                    , (IterationID 3, Map.singleton (RequestID 3) 1)
                    , (IterationID 4, Map.singleton (RequestID 5) 1)
                    ]
            stopLeft = askStr "stop left" >> pure (10 :: Int)
            stopRight = askStr "stop right" >> pure 11
            (result, _) = runResumableTest @Int @String record $
                loopM (const $ (Left <$> askStr "keep going") `selectStr` (Right <$> (stopLeft `selectStr` stopRight))) 0
        in HUnit.assertEqual "loop" (Just 10) result

    , HUnit.testCase "loop requests" $
        let record = Record
                 $ Map.fromList
                    [ (IterationID 1, Map.singleton (RequestID 1) 1)
                    , (IterationID 2, Map.singleton (RequestID 2) 1)
                    , (IterationID 3, Map.singleton (RequestID 3) 1)
                    ]
            stopLeft = askStr "stop left" >> pure (10 :: Int)
            stopRight = askStr "stop right" >> pure 11
            (_, RequestState{rsOpenRequests}) = runResumableTest @Int @String record $
                loopM (const $ (Left <$> askStr "keep going") `selectStr` (Right <$> (stopLeft `selectStr` stopRight))) 0
        in HUnit.assertEqual "loop requests"
            [ Request{rqID = RequestID 6, itID = IterationID 4, rqRequest = "stop right"}
            , Request{rqID = RequestID 5, itID = IterationID 4, rqRequest = "stop left"}
            , Request{rqID = RequestID 4, itID = IterationID 4, rqRequest = "keep going"}
            ]
            rsOpenRequests

    ]

askStr :: String -> Eff '[Resumable Int String] Int
askStr = prompt

selectStr :: Eff '[Resumable Int String] a -> Eff '[Resumable Int String] a -> Eff '[Resumable Int String] a
selectStr = select @Int @String @'[Resumable Int String]

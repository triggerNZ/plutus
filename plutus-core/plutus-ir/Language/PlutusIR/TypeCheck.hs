{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
-- | Kind/type inference/checking, mirroring Language.PlutusCore.TypeCheck
module Language.PlutusIR.TypeCheck
    (
    -- * Configuration.
      DynamicBuiltinNameTypes (..)
    , TypeCheckConfig (..)
    , tccDynamicBuiltinNameTypes
    , PLC.defConfig
    , PLC.dynamicBuiltinNameMeaningsToTypes
    -- * Type checking, extending the plc typechecker
    , checkTypeOfProgram
    ) where

import           Language.PlutusCore.Quote
import qualified Language.PlutusCore.TypeCheck        as PLC
import           Language.PlutusCore.Universe
import           Language.PlutusIR
import           Language.PlutusIR.Error
import           Language.PlutusIR.Transform.Rename   ()
import           Language.PlutusIR.TypeCheck.Internal

import           Control.Monad.Except

{- Note [Goal of PIR typechecker]

The PIR typechecker is an extension  of the PLC typechecker; whereas the PLC typechecker
works on PLC terms, the PIR typechecker works on the PIR terms. A PIR term
can be thought of as a superset of the PLC term language: it adds the `LetRec` and `LetNonRec` syntactic
constructs. Because of ths, the PIR typechecker simply extends the PLC typechecker by adding checks
for these two let constructs of PIR.

Since we already have a PIR->PLC compiler, some would say that it would suffice to first compile the PIR to PLC
and then only run the PLC typechecker. While this is mostly true, there are some reasons for having also
the PIR typechecker as an extra step on the compiler pipeline:

- The error-messages can refer to features of PIR syntax which don't exist in PLC, such as let-terms

- Although PIR is an IR and as such is not supposed to be written by humans, we do have some hand-written PIR code
in our examples/samples/testcases that we would like to make sure they typecheck.

- Our deadcode eliminator which works on PIR (in `Language.PlutusIR.Optimizer.Deadcode`) may eliminate ill-typed code, which
would turn, much to a surprise, an ill-typed program to a well-typed one.

- Some lets of the PIR user may be declared as recursive although they do not *have to* be, e.g. `let (rec) x = 3 in`
would be better written as `let (nonrec) x = 3 in`. In such cases we could signal a warning/error (NB: not implemented atm, and probably not the job of the typechecker pass).

- In general, as an extra source of (type) safety.
-}

{- NOTE [Escaping Types not user-exposed]
As per Note [PIR vs paper FIR Difference-2], the "inferred types" at PIR top-level (`program`) location are allowed to escape their scope.
However for scope-consistency reasons, we do not let the library-user see/compare those types, but only allow to
witness if the PIR program is well-typed, by using the following safe `checkTypeOfProgram :: MonadError e m => m ()`.
Using instead the "internal" `runTypeCheckM` can allow for witnessing the actual types (for debugging/testing purposes only).
-}


-- | Check that a PIR program is well-typed,
-- throwing a 'TypeError' (annotated with the value of the @ann@ argument) otherwise.
-- This function differs from PLC's same name, in that it does not allow the PIR-program's actual type to be returned,
-- because the type would then escape its scope. See NOTE [Escaping Types not user-exposed].
checkTypeOfProgram
    :: ( AsTypeError e (Term TyName Name uni ()) uni ann, AsTypeErrorExt e uni ann, MonadError e m, MonadQuote m
       , GShow uni, GEq uni, DefaultUni <: uni
       )
    => TypeCheckConfig uni -> Program TyName Name uni ann -> m ()
checkTypeOfProgram config (Program _ term) = void $ inferType config term

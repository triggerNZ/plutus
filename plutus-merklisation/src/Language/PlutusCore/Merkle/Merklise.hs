{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Language.PlutusCore.Merkle.Merklise where



import qualified Codec.Compression.GZip                          as G
import qualified Codec.Compression.GZip                          as G
import           Codec.Serialise                                 (serialise)
import           Control.Monad
import           Control.Monad.Trans.Except                      (runExceptT)
import           Crypto.Hash
import qualified Data.ByteString.Lazy                            as BSL
import           Data.Functor.Foldable
import qualified Data.List
import qualified Data.Map
import qualified Data.Set

import qualified Language.PlutusCore                             as PLC
import qualified Language.PlutusCore.Erasure.Untyped.CBOR2       as U ()
import qualified Language.PlutusCore.Erasure.Untyped.Convert     as C
import qualified Language.PlutusCore.Erasure.Untyped.DeBruijn    as D
import qualified Language.PlutusCore.Erasure.Untyped.Term        as U
import qualified Language.PlutusCore.Merkle.CBOR                 as M
import qualified Language.PlutusCore.Merkle.Constant.Typed       as M
import           Language.PlutusCore.Merkle.Convert
import qualified Language.PlutusCore.Merkle.Evaluation.CekMarker as CekMarker
import qualified Language.PlutusCore.Merkle.Evaluation.Result    as M
import           Language.PlutusCore.Merkle.Merklisable
import qualified Language.PlutusCore.Merkle.PLCSize              as PLCSize
import qualified Language.PlutusCore.Merkle.Size                 as Size
import qualified Language.PlutusCore.Merkle.Size                 as M
import           Language.PlutusCore.Merkle.Type
import           Language.PlutusCore.Pretty
import qualified Language.PlutusTx.Builtins                      as B

import           Debug.Trace


{- Numbering nodes.  We replace the annotations in an AST with unique
   Integers.  We do this by passing in a tree containing all the
   natural numbers, using the number at the root to number the current
   node and left and right subtrees to number subterms etc.  This is a
   bit profligate, but has the advantage of being purely functional. }
-}

type NodeIDs = Data.Set.Set Integer

nodynamics :: M.DynamicBuiltinNameMeanings
nodynamics = M.DynamicBuiltinNameMeanings Data.Map.empty


data Numbers = Numbers {first::Integer, left::Numbers, right:: Numbers}

makeNumbers :: Integer -> Numbers
makeNumbers n = Numbers n (makeNumbers $ 2*n) (makeNumbers $ 2*n+1)

nats :: Numbers
nats = makeNumbers 1
-- An infinite tree containing each integer >= 1 exactly once
-- nats is a bad name.

numberName :: Numbers -> PLC.Name a -> PLC.Name Integer
numberName numbers (PLC.Name x s u) = PLC.Name (first numbers) s u

numberType :: Numbers -> PLC.Type PLC.TyName a -> PLC.Type PLC.TyName Integer
numberType (Numbers q l r) =
    \case
      PLC.TyVar     _ tn      -> PLC.TyVar     q (numberTyname l tn)
      PLC.TyFun     _ ty ty'  -> PLC.TyFun     q (numberType l ty) (numberType r ty')
      PLC.TyIFix    _ ty ty'  -> PLC.TyIFix    q (numberType l ty) (numberType r ty')
      PLC.TyForall  _ tn k ty -> PLC.TyForall  q (numberTyname (left l) tn) (numberKind (right l) k) (numberType r ty)
      PLC.TyBuiltin _ tb      -> PLC.TyBuiltin q tb
      PLC.TyLam     _ tn k ty -> PLC.TyLam     q (numberTyname (left l) tn) (numberKind (right l) k) (numberType r ty)
      PLC.TyApp     _ ty ty'  -> PLC.TyApp     q (numberType l ty) (numberType r ty')

substConst :: Functor f => t -> f a -> f t
substConst = fmap . const

numberBuiltin :: Numbers -> PLC.Builtin a -> PLC.Builtin Integer
numberBuiltin (Numbers q _ _)  = substConst q
-- Maybe I'm trying to be too clever here.  Success depends on the
-- fact that builtins have no subterms, which is invisible in this
-- code.  If we did have subterms, they'd all get the same q.

numberConstant :: Numbers -> PLC.Constant a -> PLC.Constant Integer
numberConstant (Numbers q _ _) = substConst q

numberTyname :: Numbers -> PLC.TyName a -> PLC.TyName Integer
numberTyname numbers (PLC.TyName n) = PLC.TyName (numberName numbers n)

numberKind :: Numbers -> PLC.Kind a -> PLC.Kind Integer
numberKind (Numbers q l r) =
    \case
     PLC.Type _            -> PLC.Type q
     PLC.KindArrow _ k1 k2 -> PLC.KindArrow q (numberKind l k1) (numberKind r k2)

numberTerm :: Numbers -> PLC.Term PLC.TyName PLC.Name a -> PLC.Term PLC.TyName PLC.Name Integer
numberTerm (Numbers q l r) =
    \case
      PLC.Var      _ n        -> PLC.Var      q (numberName l n)
      PLC.LamAbs   _ n ty t   -> PLC.LamAbs   q (numberName l n) (numberType (left l) ty) (numberTerm r t)
      PLC.TyInst   _ t ty     -> PLC.TyInst   q (numberTerm l t) (numberType r ty)
      PLC.IWrap    _ ty1 ty t -> PLC.IWrap    q (numberType (left l) ty1) (numberType (right l) ty) (numberTerm l t)
      PLC.TyAbs    _ tn k t   -> PLC.TyAbs    q (numberTyname (left l) tn) (numberKind (right l) k) (numberTerm r t)
      PLC.Apply    _ t1 t2    -> PLC.Apply    q (numberTerm l t1) (numberTerm r t2)
      PLC.Unwrap   _ t        -> PLC.Unwrap   q (numberTerm l t)
      PLC.Error    _ ty       -> PLC.Error    q (numberType l ty)
      PLC.Constant _ c        -> PLC.Constant q (numberConstant l c)
      PLC.Builtin  _ b        -> PLC.Builtin  q (numberBuiltin l b)

numberVersion :: Numbers -> PLC.Version a -> PLC.Version Integer
numberVersion (Numbers q _ _) = substConst q

numberProgram :: PLC.Program PLC.TyName PLC.Name a -> PLC.Program PLC.TyName PLC.Name Integer
numberProgram = numProg nats
    where numProg (Numbers q l r) (PLC.Program _ v t) = PLC.Program q (numberVersion l v) (numberTerm r t)


{- Pruning unused nodes.  While we're at it, let's convert numeric annotations back to units. -}

unann :: Functor f => f a -> f()
unann x = () <$ x

type NumSet = Data.Set.Set Integer

typeId :: PLC.Type PLC.TyName Integer -> Integer
typeId = PLC.tyLoc


-- A generic type of type-pruning funtions.
-- In our case, all types will be unused, so pruners will always be applied.
type TypePruner = NumSet -> PLC.Type PLC.TyName Integer -> Type PLC.TyName ()


pruneAllTypes :: TypePruner
pruneAllTypes used ty0 =
    if not $ Data.Set.member (typeId ty0) used
    then TyPruned () (merkleHash (fromCoreType $ () <$ ty0))
    else case ty0 of
      PLC.TyVar     _ tn      -> TyVar     () (unann tn)
      PLC.TyFun     _ ty ty'  -> TyFun     () (pruneAllTypes used ty) (pruneAllTypes used ty')
      PLC.TyIFix    _ ty ty'  -> TyIFix    () (pruneAllTypes used ty) (pruneAllTypes used ty')
      PLC.TyForall  _ tn k ty -> TyForall  () (unann tn) (unann $ fromCoreKind k) (pruneAllTypes used ty)
      PLC.TyBuiltin _ tb      -> TyBuiltin () tb
      PLC.TyLam     _ tn k ty -> TyLam     () (unann tn) (unann $ fromCoreKind k) (pruneAllTypes used ty)
      PLC.TyApp     _ ty ty'  -> TyApp     () (pruneAllTypes used ty) (pruneAllTypes used ty')

pruneBigTypes :: TypePruner
pruneBigTypes used ty0 =
    if not $ Data.Set.member (typeId ty0) used
    then
        let ty1 = fromCoreType $ () <$ ty0
        in if (BSL.length $ serialise ty1) <= 32
           then ty1
           else TyPruned () (merkleHash (fromCoreType $ unann ty0))
    else case ty0 of
      PLC.TyVar     _ tn      -> TyVar     () (unann tn)
      PLC.TyFun     _ ty ty'  -> TyFun     () (pruneBigTypes used ty) (pruneBigTypes used ty')
      PLC.TyIFix    _ ty ty'  -> TyIFix    () (pruneBigTypes used ty) (pruneBigTypes used ty')
      PLC.TyForall  _ tn k ty -> TyForall  () (unann tn) (unann $ fromCoreKind k) (pruneBigTypes used ty)
      PLC.TyBuiltin _ tb      -> TyBuiltin () tb
      PLC.TyLam     _ tn k ty -> TyLam     () (unann tn) (unann $ fromCoreKind k) (pruneBigTypes used ty)
      PLC.TyApp     _ ty ty'  -> TyApp     () (pruneBigTypes used ty) (pruneBigTypes used ty')

dontPrune :: TypePruner
dontPrune _used ty = fromCoreType $ () <$ ty

termId :: PLC.Term PLC.TyName PLC.Name Integer -> Integer
termId = PLC.termLoc  -- We should rename termLoc since this isn't a location

pruneTerm :: NumSet -> TypePruner -> PLC.Term PLC.TyName PLC.Name Integer -> Term PLC.TyName PLC.Name ()
pruneTerm used pruneType t0 =
    let pruneTerm' = pruneTerm used pruneType
        pruneType' = pruneType used
    in if not $ Data.Set.member (termId t0) used
    then
        let t1 = fromCoreTerm $ () <$ t0
            threshold = 36
            -- ^ Don't Merklise anything smaller than this; note that a pruned node serialises to 36 bytes
            n = Size.termSize t1
            s = serialise (Prune () (merkleHash t1) :: Term PLC.TyName PLC.Name () )

            l = -- Debug.Trace.trace ("================================================================") $
                -- Debug.Trace.trace (show $ prettyPlcClassicDebug t0) $
                BSL.length (serialise t1)
        in if  l > threshold
           then Prune () (merkleHash t1)
           else t1
    else case t0 of
           PLC.Var      _ n         -> Var      () (unann n)
           PLC.LamAbs   _ n ty t    -> LamAbs   () (unann n) (pruneType' ty) (pruneTerm' t)
           PLC.TyInst   _ t ty      -> TyInst   () (pruneTerm' t) (pruneType' ty)
           PLC.IWrap    _ ty1 ty2 t -> IWrap    () (pruneType' ty1) (pruneType' ty2) (pruneTerm' t)
           PLC.TyAbs    _ tn k t    -> TyAbs    () (unann tn) (unann $ fromCoreKind k) (pruneTerm' t)
           PLC.Apply    _ t1 t2     -> Apply    () (pruneTerm' t1) (pruneTerm' t2)
           PLC.Unwrap   _ t         -> Unwrap   () (pruneTerm' t)
           PLC.Error    _ ty        -> Error    () (pruneType' ty)
           PLC.Constant _ c         -> Constant () (unann $ fromCoreConstant c)
           PLC.Builtin  _ b         -> Builtin  () (unann $ fromCoreBuiltin b)

pruneProgram :: Data.Set.Set Integer -> TypePruner -> PLC.Program PLC.TyName PLC.Name Integer -> Program PLC.TyName PLC.Name ()
pruneProgram used pruneType (PLC.Program _ v t) = Program () (unann v) (pruneTerm used pruneType t)

----- The following stuff should be somewhere else


type Prog = PLC.Program PLC.TyName PLC.Name ()
componentStatistics ::  Prog -> Prog -> Prog -> Prog -> String
componentStatistics validator dataVal redeemer ptx =
    let sizes prog = show (PLCSize.astInfo prog) ++ "/" ++ show (BSL.length (serialise prog))
        messages = ["\n",
                    "validator: " ++ sizes validator,
                    "dataval  : " ++ sizes dataVal,
                    "redeemer : " ++ sizes redeemer,
                    "pendingtx: " ++ sizes ptx
                   ]
    in Data.List.intercalate "\n" messages


getUsedNodes :: (Either CekMarker.CekMachineException M.EvaluationResultDef, NodeIDs) -> NodeIDs
getUsedNodes (e, nodes) =
    case e of
     Left k                             -> error $ "getUsedNodes: Left " ++ show k
     Right M.EvaluationFailure          -> nodes  -- EvaluationFailure is when we evaluate Error, which is actually OK.
     Right (M.EvaluationSuccess _term ) -> nodes

compress :: B.ByteString -> B.ByteString
compress = G.compressWith G.defaultCompressParams {G.compressLevel = G.bestCompression}

-- TODO: test that if we choose a random node and prune it, the Merkle hash of the AST doesn't change.
merklisationStatistics0 :: PLC.Program PLC.TyName PLC.Name () -> String
merklisationStatistics0 program =
    let s1 = serialise program
        numberedProgram = numberProgram program
        PLC.Program progAnn _ numberedBody = numberedProgram
        bodyAnn = PLC.termLoc numberedBody
        usedNodes =  getUsedNodes $ CekMarker.runCekWithStringBuiltins numberedProgram
        prunedProgram = pruneProgram usedNodes pruneAllTypes numberedProgram
        s2 = serialise prunedProgram
        hash1 = merkleHash $ fromCoreProgram program
        hash2 = merkleHash prunedProgram

        messages = [
         "\nBefore Merklisation",
         " AST size: " ++ (show $ PLCSize.astInfo program),
         " Serialised size = " ++ (show $ BSL.length s1) ++ " bytes",
         " Compressed size = " ++ (show $ BSL.length (compress s1)) ++ " bytes",
         "",
         "After Merklisation",
         " Number of terms used during execution = " ++ (show $ Data.Set.size usedNodes) ,
         " Remaining nodes: " ++ (show $ M.programSize prunedProgram),
         " AST size: " ++ (show $ M.astInfo prunedProgram),
         " Serialised size = " ++ (show $ BSL.length s2) ++ " bytes",
         " Compressed size = " ++ (show $ BSL.length (compress s2)) ++ " bytes",
         "",
         "Merkle hash: " ++ (show hash1),
         "Merkle hash after  pruning: " ++ (show hash2)
         ]
    in Data.List.intercalate "\n" messages



--  Want raw, raw & erased, pruned, pruned & erased.  Compressed and
--  uncompressed versions for both.  Number of nodes before and after
--  pruning in a seaparate table?

-- | Count the number of pruned nodes in a term.
prunedTerms :: Term tyname name ann -> Integer
prunedTerms = cata a where
    a VarF{}              = 0
    a (TyAbsF _ _ k t)    = t
    a (ApplyF _ t t')     = t + t'
    a (LamAbsF _ _ ty t)  = t
    a ConstantF{}         = 0
    a BuiltinF{}          = 0
    a (TyInstF _ t ty)    = t
    a (UnwrapF _ t)       = t
    a (IWrapF _ ty ty' t) = t
    a (ErrorF _ ty)       = 0
    a (PruneF _ _)        = 1

-- | Count the number of AST nodes in a program.
progPrunedTerms :: Program tyname name ann -> Integer
progPrunedTerms (Program _ _ t) = prunedTerms t


merklisationStatistics :: PLC.Program PLC.TyName PLC.Name () -> String
merklisationStatistics program =
    let numberedProgram = numberProgram program
        PLC.Program progAnn _ numberedBody = numberedProgram
        bodyAnn = PLC.termLoc numberedBody
        usedNodes =  getUsedNodes $ CekMarker.runCekWithStringBuiltins numberedProgram
        numTermNodes = PLCSize.programNumTermNodes program
        numUsedNodes = Data.Set.size usedNodes
        prunedProgram1 = pruneProgram usedNodes pruneAllTypes numberedProgram
        prunedProgram2 = pruneProgram usedNodes pruneBigTypes numberedProgram
        prunedProgram3 = pruneProgram usedNodes dontPrune numberedProgram
        strippedProgram       = C.deBruijnToIntProgram . C.deBruijnUntypedProgram . C.erasePLCProgram $ program
        strippedPrunedProgram = C.deBruijnToIntProgram . C.deBruijnUntypedProgram . C.eraseMerkleProgram $ prunedProgram1
        -- When we strip a pruned program, all types are discarded so it's irrelevant what pruning method we used
        clen = BSL.length . compress

        s1 = serialise program
        s2 = serialise strippedProgram
        s3a = serialise prunedProgram1
        s3b = serialise prunedProgram2
        s3c = serialise prunedProgram3
        s4 = serialise strippedPrunedProgram
    in "\n"
         ++ "* | " ++ show numTermNodes ++ " | " ++ show numUsedNodes ++ " | "
         ++ (show $ BSL.length s1) ++ " | " ++ (show $ BSL.length s2)
         ++ " | " ++ (show $ BSL.length s3a) ++ " | " ++ (show $ BSL.length s3b) ++ " | " ++ (show $ BSL.length s3c)
         ++ " | " ++ (show $ BSL.length s4)
         ++ " | \n"
         ++ "* | (compressed) | - | "
         ++ (show $ clen s1) ++ " | "  ++ (show $ clen s2) ++ " | "
         ++ (show $ clen s3a) ++ " | "  ++ (show $ clen s3b) ++ " | "  ++ (show $ clen s3c)
         ++ " | "  ++ (show $ clen s4) ++ " |"
         ++ "\nPruned term nodes in program: " ++ show (progPrunedTerms prunedProgram1)



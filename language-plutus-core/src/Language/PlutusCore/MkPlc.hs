{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Language.PlutusCore.MkPlc
    ( TermLike (..)
    , UniOf
    , staticBuiltinNameAsTerm
    , dynamicBuiltinNameAsTerm
    , mkTyBuiltin
    , mkConstant
    , VarDecl (..)
    , TyVarDecl (..)
    , TyDecl (..)
    , mkVar
    , mkTyVar
    , tyDeclVar
    , Def (..)
    , embed
    , TermDef
    , TypeDef
    , FunctionType (..)
    , FunctionDef (..)
    , functionTypeToType
    , functionDefToType
    , functionDefVarDecl
    , mkFunctionDef
    , mkImmediateLamAbs
    , mkImmediateTyAbs
    , mkIterTyForall
    , mkIterTyLam
    , mkIterApp
    , mkIterTyFun
    , mkIterLamAbs
    , mkIterInst
    , mkIterTyAbs
    , mkIterTyApp
    , mkIterKindArrow
    ) where

import           Prelude                               hiding (error)

import           Language.PlutusCore.Universe
import           Language.PlutusCore.Core

import           Data.List                             (foldl')
import           GHC.Generics                          (Generic)

-- | A final encoding for Term, to allow PLC terms to be used transparently as PIR terms.
class TermLike term tyname name uni | term -> tyname, term -> name, term -> uni where
    var      :: ann -> name -> term ann
    tyAbs    :: ann -> tyname -> Kind ann -> term ann -> term ann
    lamAbs   :: ann -> name -> Type tyname uni ann -> term ann -> term ann
    apply    :: ann -> term ann -> term ann -> term ann
    constant :: ann -> Some (ValueOf uni) -> term ann
    builtin  :: ann -> BuiltinName ann -> term ann
    tyInst   :: ann -> term ann -> Type tyname uni ann -> term ann
    unwrap   :: ann -> term ann -> term ann
    iWrap    :: ann -> Type tyname uni ann -> Type tyname uni ann -> term ann -> term ann
    error    :: ann -> Type tyname uni ann -> term ann
    termLet  :: ann -> TermDef term tyname name uni ann -> term ann -> term ann
    typeLet  :: ann -> TypeDef tyname uni ann -> term ann -> term ann

-- | Lift a 'BuiltinName' to 'Term'.
staticBuiltinNameAsTerm :: TermLike term tyname name uni => StaticBuiltinName -> term ()
staticBuiltinNameAsTerm = builtin () . StaticBuiltinName ()

-- | Lift a 'DynamicBuiltinName' to 'Term'.
dynamicBuiltinNameAsTerm :: TermLike term tyname name uni => DynamicBuiltinName -> term ()
dynamicBuiltinNameAsTerm = builtin () . DynBuiltinName ()

-- | Embed a type from a universe into a PLC type.
mkTyBuiltin
    :: forall a uni tyname ann. uni `Includes` a
    => ann -> Type tyname uni ann
mkTyBuiltin ann = TyBuiltin ann . Some . TypeIn $ knownUni @uni @a

-- | Embed a Haskell value into a PLC term.
mkConstant
    :: forall a uni term tyname name ann. (TermLike term tyname name uni, uni `Includes` a)
    => ann -> a -> term ann
mkConstant ann = constant ann . someValue

instance TermLike (Term tyname name uni) tyname name uni where
    var      = Var
    tyAbs    = TyAbs
    lamAbs   = LamAbs
    apply    = Apply
    constant = Constant
    builtin  = Builtin
    tyInst   = TyInst
    unwrap   = Unwrap
    iWrap    = IWrap
    error    = Error
    termLet  = mkImmediateLamAbs
    typeLet  = mkImmediateTyAbs

embed :: TermLike term tyname name uni => Term tyname name uni ann -> term ann
embed = \case
    Var a n           -> var a n
    TyAbs a tn k t    -> tyAbs a tn k (embed t)
    LamAbs a n ty t   -> lamAbs a n ty (embed t)
    Apply a t1 t2     -> apply a (embed t1) (embed t2)
    Constant a c      -> constant a c
    Builtin a bi      -> builtin a bi
    TyInst a t ty     -> tyInst a (embed t) ty
    Error a ty        -> error a ty
    Unwrap a t        -> unwrap a (embed t)
    IWrap a ty1 ty2 t -> iWrap a ty1 ty2 (embed t)

-- | A "variable declaration", i.e. a name and a type for a variable.
data VarDecl tyname name uni ann = VarDecl
    { varDeclAnn  :: ann
    , varDeclName :: name
    , varDeclType :: Type tyname uni ann
    } deriving (Functor, Show, Generic)

-- | Make a 'Var' referencing the given 'VarDecl'.
mkVar :: TermLike term tyname name uni => ann -> VarDecl tyname name uni ann -> term ann
mkVar ann = var ann . varDeclName

-- | A "type variable declaration", i.e. a name and a kind for a type variable.
data TyVarDecl tyname ann = TyVarDecl
    { tyVarDeclAnn  :: ann
    , tyVarDeclName :: tyname
    , tyVarDeclKind :: Kind ann
    } deriving (Functor, Show, Generic)

-- | Make a 'TyVar' referencing the given 'TyVarDecl'.
mkTyVar :: ann -> TyVarDecl tyname ann -> Type tyname uni ann
mkTyVar ann = TyVar ann . tyVarDeclName

-- | A "type declaration", i.e. a kind for a type.
data TyDecl tyname uni ann = TyDecl
    { tyDeclAnn  :: ann
    , tyDeclType :: Type tyname uni ann
    , tyDeclKind :: Kind ann
    } deriving (Functor, Show, Generic)

tyDeclVar :: TyVarDecl tyname ann -> TyDecl tyname uni ann
tyDeclVar (TyVarDecl ann name kind) = TyDecl ann (TyVar ann name) kind

-- | A definition. Pretty much just a pair with more descriptive names.
data Def var val = Def
    { defVar :: var
    , defVal :: val
    } deriving (Show, Eq, Ord, Generic)

-- | A term definition as a variable.
type TermDef term tyname name uni ann = Def (VarDecl tyname name uni ann) (term ann)
-- | A type definition as a type variable.
type TypeDef tyname uni ann = Def (TyVarDecl tyname ann) (Type tyname uni ann)

-- | The type of a PLC function.
data FunctionType tyname uni ann = FunctionType
    { _functionTypeAnn :: ann                  -- ^ An annotation.
    , _functionTypeDom :: Type tyname uni ann  -- ^ The domain of a function.
    , _functionTypeCod :: Type tyname uni ann  -- ^ The codomain of the function.
    }

-- Should we parameterize 'VarDecl' by @ty@ rather than @tyname@, so that we can define
-- 'FunctionDef' as 'TermDef FunctionType tyname name uni ann'?
-- Perhaps we even should define general 'Decl' and 'Def' that cover all of the cases?
-- | A PLC function.
data FunctionDef term tyname name uni ann = FunctionDef
    { _functionDefAnn  :: ann                          -- ^ An annotation.
    , _functionDefName :: name                         -- ^ The name of a function.
    , _functionDefType :: FunctionType tyname uni ann  -- ^ The type of the function.
    , _functionDefTerm :: term ann                     -- ^ The definition of the function.
    }

-- | Convert a 'FunctionType' to the corresponding 'Type'.
functionTypeToType :: FunctionType tyname uni ann -> Type tyname uni ann
functionTypeToType (FunctionType ann dom cod) = TyFun ann dom cod

-- | Get the type of a 'FunctionDef'.
functionDefToType :: FunctionDef term tyname name uni ann -> Type tyname uni ann
functionDefToType (FunctionDef _ _ funTy _) = functionTypeToType funTy

-- | Convert a 'FunctionDef' to a 'VarDecl'. I.e. ignore the actual term.
functionDefVarDecl :: FunctionDef term tyname name uni ann -> VarDecl tyname name uni ann
functionDefVarDecl (FunctionDef ann name funTy _) = VarDecl ann name $ functionTypeToType funTy

-- | Make a 'FunctioDef'. Return 'Nothing' if the provided type is not functional.
mkFunctionDef
    :: ann
    -> name
    -> Type tyname uni ann
    -> term ann
    -> Maybe (FunctionDef term tyname name uni ann)
mkFunctionDef annName name (TyFun annTy dom cod) term =
    Just $ FunctionDef annName name (FunctionType annTy dom cod) term
mkFunctionDef _       _    _                     _    = Nothing

-- | Make a "let-binding" for a term as an immediately applied lambda abstraction.
mkImmediateLamAbs
    :: TermLike term tyname name uni
    => ann
    -> TermDef term tyname name uni ann
    -> term ann -- ^ The body of the let, possibly referencing the name.
    -> term ann
mkImmediateLamAbs ann1 (Def (VarDecl ann2 name ty) bind) body =
    apply ann1 (lamAbs ann2 name ty body) bind

-- | Make a "let-binding" for a type as an immediately instantiated type abstraction. Note: the body must be a value.
mkImmediateTyAbs
    :: TermLike term tyname name uni
    => ann
    -> TypeDef tyname uni ann
    -> term ann -- ^ The body of the let, possibly referencing the name.
    -> term ann
mkImmediateTyAbs ann1 (Def (TyVarDecl ann2 name k) bind) body =
    tyInst ann1 (tyAbs ann2 name k body) bind

-- | Make an iterated application.
mkIterApp
    :: TermLike term tyname name uni
    => ann
    -> term ann -- ^ @f@
    -> [term ann] -- ^@[ x0 ... xn ]@
    -> term ann -- ^ @[f x0 ... xn ]@
mkIterApp ann = foldl' (apply ann)

-- | Make an iterated instantiation.
mkIterInst
    :: TermLike term tyname name uni
    => ann
    -> term ann -- ^ @a@
    -> [Type tyname uni ann] -- ^ @ [ x0 ... xn ] @
    -> term ann -- ^ @{ a x0 ... xn }@
mkIterInst ann = foldl' (tyInst ann)

-- | Lambda abstract a list of names.
mkIterLamAbs
    :: TermLike term tyname name uni
    => [VarDecl tyname name uni ann]
    -> term ann
    -> term ann
mkIterLamAbs args body =
    foldr (\(VarDecl ann name ty) acc -> lamAbs ann name ty acc) body args

-- | Type abstract a list of names.
mkIterTyAbs
    :: TermLike term tyname name uni
    => [TyVarDecl tyname ann]
    -> term ann
    -> term ann
mkIterTyAbs args body =
    foldr (\(TyVarDecl ann name kind) acc -> tyAbs ann name kind acc) body args

-- | Make an iterated type application.
mkIterTyApp
    :: ann
    -> Type tyname uni ann -- ^ @f@
    -> [Type tyname uni ann] -- ^ @[ x0 ... xn ]@
    -> Type tyname uni ann -- ^ @[ f x0 ... xn ]@
mkIterTyApp ann = foldl' (TyApp ann)

-- | Make an iterated function type.
mkIterTyFun
    :: ann
    -> [Type tyname uni ann]
    -> Type tyname uni ann
    -> Type tyname uni ann
mkIterTyFun ann tys target = foldr (\ty acc -> TyFun ann ty acc) target tys

-- | Universally quantify a list of names.
mkIterTyForall
    :: [TyVarDecl tyname ann]
    -> Type tyname uni ann
    -> Type tyname uni ann
mkIterTyForall args body =
    foldr (\(TyVarDecl ann name kind) acc -> TyForall ann name kind acc) body args

-- | Lambda abstract a list of names.
mkIterTyLam
    :: [TyVarDecl tyname ann]
    -> Type tyname uni ann
    -> Type tyname uni ann
mkIterTyLam args body =
    foldr (\(TyVarDecl ann name kind) acc -> TyLam ann name kind acc) body args

-- | Make an iterated function kind.
mkIterKindArrow
    :: ann
    -> [Kind ann]
    -> Kind ann
    -> Kind ann
mkIterKindArrow ann kinds target = foldr (KindArrow ann) target kinds

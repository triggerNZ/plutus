-- Serialise without annotations. Ignore deserialisation.

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Serialise instances for Plutus Core types. Make sure to read the Note [Stable encoding of PLC]
-- before touching anything in this file.
module Language.PlutusCore.CBOR2 () where

import           Language.PlutusCore.Core
import           Language.PlutusCore.DeBruijn
import           Language.PlutusCore.Error
-- import           Language.PlutusCore.Lexer      (AlexPosn)
import           Language.PlutusCore.Lexer.Type
import           Language.PlutusCore.MkPlc      (TyVarDecl (..), VarDecl (..))
import           Language.PlutusCore.Name
import           PlutusPrelude

import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import           Codec.Serialise
import qualified Data.ByteString.Lazy           as BSL
import           Data.Functor.Foldable          hiding (fold)

{- Note [Stable encoding of PLC]
READ THIS BEFORE TOUCHING ANYTHING IN THIS FILE

We need the encoding of PLC on the blockchain to be *extremely* stable. It *must not* change
arbitrarily, otherwise we'll be unable to read back old transactions and validate them.

Consequently we don't use the derivable instances of `Serialise` for the PLC types that go
on the chain. (Also, the CBOR produced by those instances is more than 60% larger than that
produced by the instances below.)

However, the instances in this file *are* constrained by instances for names, type names,
and annotations. What's to stop the instances for *those* changing, thus changing
the overall encoding on the chain?

The answer is that what goes on the chain is *always* a `Program TyName Name ()`. The instances
for `TyName` and `Name` are nailed down here, and the instance for `()` is standard.

However, having this flexibility allows us to encode e.g. PLC with substantial annotations
(like position information) in situation where the stability is *not* critical, such as
for testing.
-}


{- [Note: Encoding/decoding constructor tags]
   Use `encodeConstructorTag` and `decodeConstructorTag` to encode/decode
   tags representing constructors.  These are just aliases for
   `encodeWord` and `decodeWord`. Note that `encodeWord` is careful about
   sizes and will only use one byte for the tags we have here.  NB: Don't
   use encodeTag or decodeTag; those are for use with a fixed set of CBOR
   tags with predefined meanings which we shouldn't interfere with.
   See http://hackage.haskell.org/package/serialise.
-}
encodeConstructorTag :: Word -> Encoding
encodeConstructorTag = encodeWord

decodeConstructorTag :: Decoder s Word
decodeConstructorTag = decodeWord

instance Serialise TypeBuiltin where
    encode bi = case bi of
        TyByteString -> encodeConstructorTag 0
        TyInteger    -> encodeConstructorTag 1
        TyString     -> encodeConstructorTag 2

    decode = go =<< decodeConstructorTag
        where go 0 = pure TyByteString
              go 1 = pure TyInteger
              go 2 = pure TyString
              go _ = fail "Failed to decode TypeBuiltin"

instance Serialise BuiltinName where
    encode bi =
        let i = case bi of
                AddInteger           -> 0
                SubtractInteger      -> 1
                MultiplyInteger      -> 2
                DivideInteger        -> 3
                RemainderInteger     -> 4
                LessThanInteger      -> 5
                LessThanEqInteger    -> 6
                GreaterThanInteger   -> 7
                GreaterThanEqInteger -> 8
                EqInteger            -> 9
                Concatenate          -> 10
                TakeByteString       -> 11
                DropByteString       -> 12
                SHA2                 -> 13
                SHA3                 -> 14
                VerifySignature      -> 15
                EqByteString         -> 16
                QuotientInteger      -> 17
                ModInteger           -> 18
                LtByteString         -> 19
                GtByteString         -> 20
        in encodeConstructorTag i

    decode = go =<< decodeConstructorTag
        where go 0  = pure AddInteger
              go 1  = pure SubtractInteger
              go 2  = pure MultiplyInteger
              go 3  = pure DivideInteger
              go 4  = pure RemainderInteger
              go 5  = pure LessThanInteger
              go 6  = pure LessThanEqInteger
              go 7  = pure GreaterThanInteger
              go 8  = pure GreaterThanEqInteger
              go 9  = pure EqInteger
              go 10 = pure Concatenate
              go 11 = pure TakeByteString
              go 12 = pure DropByteString
              go 13 = pure SHA2
              go 14 = pure SHA3
              go 15 = pure VerifySignature
              go 16 = pure EqByteString
              go 17 = pure QuotientInteger
              go 18 = pure ModInteger
              go 19 = pure LtByteString
              go 20 = pure GtByteString
              go _  = fail "Failed to decode BuiltinName"

instance Serialise Unique where
    encode (Unique i) = encodeInt i
    decode = Unique <$> decodeInt

instance Serialise (Name ()) where
    -- TODO: should we encode the name or not?
    encode (Name () txt u) = encode txt <> encode u
    decode = Name <$> pure() <*> decode <*> decode

instance Serialise (TyName ()) where
    encode (TyName n) = encode n
    decode = TyName <$> decode

instance Serialise (Version ()) where
    encode (Version () n n' n'') = fold [ encode n, encode n', encode n'' ]
    decode = Version <$> pure () <*> decode <*> decode <*> decode

instance Serialise (Kind ()) where
    encode = cata a where
        a (TypeF ())           = encodeConstructorTag 0
        a (KindArrowF () k k') = fold [ encodeConstructorTag 1, k , k' ]

    decode = go =<< decodeConstructorTag
        where go 0 = Type <$> pure()
              go 1 = KindArrow <$> pure()  <*> decode <*> decode
              go _ = fail "Failed to decode Kind ()"

instance Serialise (tyname ()) => Serialise (Type tyname ()) where
    encode = cata a where
        a (TyVarF () tn)        = encodeConstructorTag 0 <>  encode tn
        a (TyFunF () t t')      = encodeConstructorTag 1 <>  t <> t'
        a (TyIFixF () pat arg)  = encodeConstructorTag 2 <>  pat <> arg
        a (TyForallF () tn k t) = encodeConstructorTag 3 <>  encode tn <> encode k <> t
        a (TyBuiltinF () con)   = encodeConstructorTag 4 <>  encode con
        a (TyLamF () n k t)     = encodeConstructorTag 5 <>  encode n <> encode k <> t
        a (TyAppF () t t')      = encodeConstructorTag 6 <>  t <> t'

    decode = go =<< decodeConstructorTag
        where go 0 = TyVar <$> pure () <*> decode
              go 1 = TyFun <$> pure () <*> decode <*> decode
              go 2 = TyIFix <$> pure ()  <*> decode <*> decode
              go 3 = TyForall <$> pure () <*> decode <*> decode <*> decode
              go 4 = TyBuiltin <$> pure () <*> decode
              go 5 = TyLam <$> pure () <*> decode <*> decode <*> decode
              go 6 = TyApp <$> pure () <*> decode <*> decode
              go _ = fail "Failed to decode Type TyName ()"

instance Serialise DynamicBuiltinName where
    encode (DynamicBuiltinName name) = encode name
    decode = DynamicBuiltinName <$> decode

instance Serialise (Builtin ()) where
    encode (BuiltinName () bn)     = encodeConstructorTag 0 <> encode bn
    encode (DynBuiltinName () dbn) = encodeConstructorTag 1 <> encode dbn

    decode = go =<< decodeConstructorTag
        where go 0 = BuiltinName <$> pure ()<*> decode
              go 1 = DynBuiltinName <$> pure () <*> decode
              go _ = fail "Failed to decode Builtin ()"


instance Serialise (Constant ()) where
    encode (BuiltinInt () i) = fold [ encodeConstructorTag 0, encodeInteger i ]
    encode (BuiltinBS () bs) = fold [ encodeConstructorTag 1, encodeBytes (BSL.toStrict bs) ]
    encode (BuiltinStr () s) = encodeConstructorTag 2 <>  encode s
    decode = go =<< decodeConstructorTag
        where go 0 = BuiltinInt <$> pure () <*> decodeInteger
              go 1 = BuiltinBS <$> pure () <*> fmap BSL.fromStrict decodeBytes
              go 2 = BuiltinStr <$> pure () <*> decode
              go _ = fail "Failed to decode Constant ()"

instance (Serialise (tyname ()), Serialise (name ())) => Serialise (Term tyname name ()) where
    encode = cata a where
        a (VarF () n)           = encodeConstructorTag 0 <>  encode n
        a (TyAbsF () tn k t)    = encodeConstructorTag 1 <>  encode tn <> encode k <> t
        a (LamAbsF () n ty t)   = encodeConstructorTag 2 <>  encode n <> encode ty <> t
        a (ApplyF () t t')      = encodeConstructorTag 3 <>  t <> t'
        a (ConstantF () c)      = encodeConstructorTag 4 <>  encode c
        a (TyInstF () t ty)     = encodeConstructorTag 5 <>  t <> encode ty
        a (UnwrapF () t)        = encodeConstructorTag 6 <>  t
        a (IWrapF () pat arg t) = encodeConstructorTag 7 <>  encode pat <> encode arg <> t
        a (ErrorF () ty)        = encodeConstructorTag 8 <>  encode ty
        a (BuiltinF () bi)      = encodeConstructorTag 9 <>  encode bi

    decode = go =<< decodeConstructorTag
        where go 0 = Var <$> pure ()  <*> decode
              go 1 = TyAbs <$> pure () <*> decode <*> decode <*> decode
              go 2 = LamAbs <$> pure () <*> decode <*> decode <*> decode
              go 3 = Apply <$> pure ()  <*> decode <*> decode
              go 4 = Constant <$> pure () <*> decode
              go 5 = TyInst <$> pure ()  <*> decode <*> decode
              go 6 = Unwrap <$> pure ()  <*> decode
              go 7 = IWrap <$> pure ()  <*> decode <*> decode <*> decode
              go 8 = Error <$> pure ()  <*> decode
              go 9 = Builtin <$> pure ()  <*> decode
              go _ = fail "Failed to decode Term TyName Name ()"

instance  (Serialise (tyname ())
         , Serialise (name ())
         ) => Serialise (VarDecl tyname name ()) where
    encode (VarDecl t name tyname ) = encode t <> encode name <> encode tyname
    decode = VarDecl <$> decode <*> decode <*> decode

instance Serialise (tyname ())  => Serialise (TyVarDecl tyname ()) where
    encode (TyVarDecl t tyname kind) = encode t <> encode tyname <> encode kind
    decode = TyVarDecl <$> decode <*> decode <*> decode

instance ( Serialise (tyname ())
         , Serialise (name ())
         ) => Serialise (Program tyname name ()) where
    encode (Program () v t) =  encode v <> encode t
    decode = Program <$> pure () <*> decode <*> decode

deriving newtype instance (Serialise a) => Serialise (Normalized a)

instance Serialise a => Serialise (Token a)
-- instance Serialise AlexPosn
instance Serialise Keyword
instance Serialise Special

deriving instance Serialise Index

instance Serialise (DeBruijn ()) where
    encode (DeBruijn () txt i) =  encode txt <> encode i
    decode = DeBruijn <$> pure() <*> decode <*> decode

instance Serialise (TyDeBruijn ()) where
    encode (TyDeBruijn n) = encode n
    decode = TyDeBruijn <$> decode

instance Serialise (ParseError ())
instance Serialise (tyname ()) => Serialise (ValueRestrictionError tyname ())
instance (Serialise (tyname ()), Serialise (name ())) =>
            Serialise (NormCheckError tyname name ())
instance Serialise (UniqueError ())
instance Serialise UnknownDynamicBuiltinNameError
instance Serialise (InternalTypeError ())
instance Serialise (TypeError ())
instance Serialise (Error ())

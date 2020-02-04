{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Marlex where

import           Control.Applicative    ((<$>))
import Control.Monad.State
import qualified Data.Char as Char
import           Data.List as List
import           Data.Maybe
import           Data.Void
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.List.NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import           Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec as Megaparsec
import Text.Megaparsec.Debug
import Text.Megaparsec.Char
import Text.Megaparsec.Pos
import Debug.Trace

import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char
import qualified Data.List.NonEmpty as NonEmpty
import Text.Megaparsec
import Control.Monad (void, when)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
-- import Language.Marlowe (Contract)
import qualified Language.Marlowe as Marlowe

type Parser = Parsec Void Text

data Lit  = I Integer | S Text
  deriving (Eq,Show)

data Bind = Bind Text Expr
  deriving (Eq,Show)

data Expr = Lit Lit
          | Ident Text
          | App Expr [Expr]
          | Lam [Text] Expr
          | Let [Bind] Expr
  deriving (Eq,Show)


data Val = VI Integer
         | VS Text
         | VCon Text [Val]
         | VClosure Env Expr
  deriving (Eq,Show)

data Def = Def Text [Text] Expr
  deriving (Eq,Show)


data Marlex = Marlex [Def] Expr
  deriving (Eq,Show)


-- type Env = [(Text, Expr)]
type Env = [(Text, Val)]

defaultEnv = [
      ("Close", VCon "Close" [])
    , ("When",  VCon "When"  [])
    , ("Pay",   VCon "Pay"   [])
    , ("Slot",  VCon "Slot"  [])
    ]

evalM :: Env -> Marlex -> Either String Marlowe.Contract
evalM env (Marlex defs expr) = let
    -- newEnv = foldl addEnv defs ++ env
    in convertContract (eval defaultEnv expr)


addEnv :: (Text, Expr) -> Env -> Env
addEnv (n, e) env = (n, eval env e) : env


eval :: Env -> Expr -> Val
eval env expr = case expr of
    Lit (I i) -> VI i
    Ident name -> lookupEnv env name
    App f e -> apply (eval env f) (fmap (eval env) e)
    Lam params body -> VClosure env body
    Let binds body -> VClosure env body

apply f args = case f of
    VCon n as -> VCon n (as ++ args)
    VClosure env (Lam params body) -> eval (List.zip params args  ++ env) body

convertContract val = case val of
    VCon "Close" [] -> Right Marlowe.Close
    {- VCon "Pay" [_, t, f] -> do
        tt <- convertContract t
        ff <- convertContract f
        return $ Pay (Account 0 ) tt ff -}
    VCon "When" [cases, VCon "Slot" [VI i], cont] -> do
        -- cs <- convertCases cases
        c  <- convertContract cont
        return $ Marlowe.When [] (Marlowe.Slot i) c
    _ -> Left (show val)

convertValue val = case val of
    VCon "Constant" [VI i] -> Right (Marlowe.Constant i)
    _ -> Left (show val)


convertCases val = case val of
    VCon "Constant" [VI i] -> Right (Marlowe.Constant i)
    _ -> Left (show val)

lookupEnv env name = fromMaybe (error ("not found " ++ show name)) $ lookup name env


keywords = ["data", "if", "then", "else", "in", "let"]
ops = ["=="]


expr :: Parser Expr
expr = try app <|> factor  <?> "expression"


app :: Parser Expr
app = dbg "app" $ do
    f <- applicable
    exprs <- some term
    return $ App f exprs


factor :: Parser Expr
factor = dbg "factor" (lam <|> try letin <|> term)


term :: Parser Expr
term = literal <|> applicable


applicable :: Parser Expr
applicable = ident <|> parens expr


lam :: Parser Expr
lam = dbg "lam" $ do
    symbol "\\"
    args <- some identifier
    symbol "->"
    body <- expr
    return $ Lam args body


letin :: Parser Expr
letin = do
    reserved "let"
    binds <- braces (semiSep letbind)
    reserved "in"
    body <- expr
    return $ Let binds body


letbind :: Parser Bind
letbind = do
    name <- identifier
    symbol "="
    body <- expr
    return $ Bind name body


ident :: Parser Expr
ident = Ident <$> identifier


literal :: Parser Expr
literal = integerLiteral <|> stringLit


sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = L.space (void space1) lineComment blockComment
  where lineComment = string "--" *> void (takeWhileP (Just "character") (/= '\n'))
        blockComment = L.skipBlockComment "{-" "-}"

identChar = alphaNumChar

lexeme = L.lexeme sc

symbol = L.symbol sc

integer = lexeme (try (char '0' *> char' 'x' *> L.hexadecimal)
  <|> try (char '0' *> char' 'o' *> L.octal)
  <|> try L.decimal)

stringLiteral :: Parser Text
stringLiteral = do
    char '"'
    l <- manyTill L.charLiteral (char '"')
    return $ T.pack l

parens = between (symbol "(") (symbol ")")
brackets  = between (symbol "[") (symbol "]")
braces  = between (symbol "{") (symbol "}")
comma = symbol ","
semi = symbol ";"
commaSep p  = p `sepBy` comma
semiSep  p  = p `sepBy` semi

reserved :: Text -> Parser ()
reserved w = string w *> notFollowedBy identChar *> sc

reservedOp :: Text -> Parser ()
reservedOp w = string w *> notFollowedBy opChar *> sc

identifier :: Parser Text
identifier = lexeme $ try $ do
    ident <- identifierOrReserved
    when (ident `elem` keywords) $ unexpected . Label . NonEmpty.fromList $ "reserved " ++ (T.unpack ident)
    when (ident == "_") $ unexpected . Label . NonEmpty.fromList $ "wildcard"
    return ident


identifierOrReserved = lexeme $ try $ do
    c <- satisfy (\ch -> isAlpha ch || ch == '_')
    T.cons c <$> idrest
  where
    idrest = takeWhileP Nothing (\ch -> isAlphaNum ch || ch == '_' || ch == '$')

opChar :: Parser Char
opChar = oneOf ("!$%&*+./<=>?@\\^|-~" :: String)

operator :: Parser Text
operator = do
    op <- some opChar
    lexeme $ return $ T.pack op



integerLiteral :: Parser Expr
integerLiteral = Lit <$> integerLit

integerLit :: Parser Lit
integerLit = I . fromIntegral <$> integer


stringLit :: Parser Expr
stringLit = Lit . S <$> lexeme stringLiteral


letins :: Parser Expr
letins = do
    reserved "let"
    reserved "{"
    binds <- semiSep $ do
        var <- identifier
        reservedOp "="
        val <- expr
        return (Bind var val)
    reserved "}"
    reserved "in"
    body <- expr
    return $ (Let binds body)

contents p = between sc eof p

parseWith :: Parser a -> Text ->Either (Megaparsec.ParseErrorBundle Text Void) a
parseWith parser s = parse (contents parser) "<stdin>" s

parseExpr = parseWith expr

-- parseToplevelFilename fileName s = parse (contents toplevel) fileName s

-- parseToplevel s = parseToplevelFilename "<stdin>" s

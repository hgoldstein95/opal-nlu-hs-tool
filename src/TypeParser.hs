module TypeParser where

import Ast
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef
  { Token.commentStart = "/*"
  , Token.commentEnd = "*/"
  , Token.commentLine = "//"
  , Token.identStart = letter
  , Token.identLetter = alphaNum
  , Token.reservedNames = ["alias", "free-text", "keywords", "trait"]
  , Token.reservedOpNames = ["=", ":", "|", "\"", "<", ">"]
  }

lexer = Token.makeTokenParser languageDef
ident = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
semi = Token.semi lexer
braces = Token.braces lexer
commaSep = Token.commaSep lexer
barSep x = sepBy1 x (whiteSpace >> reservedOp "|" >> whiteSpace)
quotes = between (reservedOp "\"") (reservedOp "\"")
angles = between (reservedOp "<") (reservedOp ">")
whiteSpace = Token.whiteSpace lexer

parseDecls :: Parser [Decl]
parseDecls =
  many $ parseFreeText <|> parseKeywords <|> parseTrait <|> parseAlias

parseFreeText :: Parser Decl
parseFreeText = do
  reserved "free-text"
  x <- ident
  return $ FreeText x

parseKeywords :: Parser Decl
parseKeywords = do
  reserved "keywords"
  x <- ident
  reservedOp "="
  ls <- barSep (quotes ident)
  return $ Keywords x ls

parseTrait :: Parser Decl
parseTrait = do
  reserved "trait"
  x <- ident
  reservedOp "="
  ps <-
    barSep $ do
      s <- angles ident
      t <- parseTy
      return $ (s, t)
  return $ Trait x ps

parseTy :: Parser Ty
parseTy = parseDef <|> parseRec

parseDef :: Parser Ty
parseDef = do
  x <- ident
  return $ Def x

parseRec :: Parser Ty
parseRec = do
  ls <-
    braces $
    commaSep $ do
      l <- ident
      reservedOp ":"
      t <- parseTy
      return $ (l, t)
  return $ Rec ls

parseAlias :: Parser Decl
parseAlias = do
  reserved "alias"
  x <- ident
  reservedOp "="
  t <- parseTy
  return $ Alias x t

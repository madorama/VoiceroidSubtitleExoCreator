{-# LANGUAGE Strict #-}

module Config.Parser ( parseConfig ) where

import qualified Data.ByteString.UTF8 as BS8
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Text.Parsec.String
import Config.Ast

-- Lexer

reservedNames :: [String]
reservedNames =
  [ "workspace"
  , "x", "y", "size", "font", "color", "shadow", "position"
  , "top", "middle", "bottom", "left", "center", "right"
  ]

configStyle =
  emptyDef
    { T.reservedNames = reservedNames
    , T.caseSensitive = False
    }
lexer = T.makeTokenParser configStyle
lexeme = T.lexeme lexer
integer = lexeme $ T.integer lexer
float = lexeme $ T.float lexer
symbol = T.symbol lexer
stringLiteral = T.stringLiteral lexer
reserved = T.reserved lexer
parens = T.parens lexer
braces = T.braces lexer
comma = T.comma lexer
colon = T.colon lexer
commaSep = T.commaSep lexer
ws = T.whiteSpace lexer

colorP :: Parser String
colorP =
  lexeme (symbol "#" *> count 6 hexDigit)

numberP :: Parser Double
numberP =
  try float
  <|> (fromInteger <$> integer)

pos :: Parser Loc
pos = positionToLoc <$> getPosition

-- Parser

parseConfig :: SourceName -> BS8.ByteString -> Either ParseError [Ast]
parseConfig = (. BS8.toString) . parse (ws *> many configP <* eof)

configP :: Parser Ast
configP = choice
  [ Workspace <$> pos <*> try (reserved "workspace" *> stringLiteral)
  , Config
      <$> pos
      <*> try stringLiteral
      <*> optionMaybe (parens stringLiteral)
      <*> braces (commaSep valueP)
  ]

valueP :: Parser Value
valueP = choice
  [ try $ X <$> value "x" numberP
  , try $ Y <$> value "y" numberP
  , try $ Color <$> value "color" colorP
  , try $ Shadow <$> value "shadow" colorP
  , try $ Font <$> value "font" stringLiteral
  , try $ Size <$> value "size" integer
  , try $ Position <$> value "position" (symbol "(" *> horizontalP) <*> (comma *> verticalP <* symbol ")")
  ]
  where
    value name p = reserved name *> colon *> p

verticalP :: Parser Vertical
verticalP = choice
  [ try $ VTop <$ reserved "top"
  , try $ VMiddle <$ reserved "middle"
  , try $ VBottom <$ reserved "bottom"
  ]

horizontalP :: Parser Horizontal
horizontalP = choice
  [ try $ HLeft <$ reserved "left"
  , try $ HCenter <$ reserved "center"
  , try $ HRight <$ reserved "right"
  ]

module HW3.Parser (parse) where

import Control.Applicative (many, optional, (<|>))
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Data.ByteString as B (pack)
import qualified Data.Text as T (Text, pack)
import Data.Void (Void)
import Data.Word (Word8)
import HW3.Base
import Text.Megaparsec
  ( ParseErrorBundle,
    Parsec,
    between,
    choice,
    eof,
    manyTill,
    notFollowedBy,
    runParser,
    sepBy,
    sepBy1,
    try, 
    satisfy,
  )
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (isAlphaNum, isAlpha)

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between space eof operatorParser) ""

type Parser = Parsec Void String

funParser :: Parser HiFun
funParser =
  choice
    [ HiFunDiv <$ string "div",
      HiFunMul <$ string "mul",
      HiFunAdd <$ string "add",
      HiFunSub <$ string "sub",
      HiFunNotLessThan <$ string "not-less-than",
      HiFunNotGreaterThan <$ string "not-greater-than",
      HiFunNotEquals <$ string "not-equals",
      HiFunNot <$ string "not",
      HiFunAnd <$ string "and",
      HiFunOr <$ string "or",
      HiFunLessThan <$ string "less-than",
      HiFunGreaterThan <$ string "greater-than",
      HiFunEquals <$ string "equals",
      HiFunIf <$ string "if",
      HiFunLength <$ string "length",
      HiFunToUpper <$ string "to-upper",
      HiFunToLower <$ string "to-lower",
      HiFunReverse <$ string "reverse",
      HiFunTrim <$ string "trim",
      HiFunList <$ string "list",
      HiFunRange <$ string "range",
      HiFunFold <$ string "fold",
      HiFunPackBytes <$ string "pack-bytes",
      HiFunUnpackBytes <$ string "unpack-bytes",
      HiFunZip <$ string "zip",
      HiFunUnzip <$ string "unzip",
      HiFunEncodeUtf8 <$ string "encode-utf8",
      HiFunDecodeUtf8 <$ string "decode-utf8",
      HiFunSerialise <$ string "serialise",
      HiFunDeserialise <$ string "deserialise",
      HiFunRead <$ string "read",
      HiFunWrite <$ string "write",
      HiFunMkDir <$ string "mkdir",
      HiFunChDir <$ string "cd",
      HiFunParseTime <$ string "parse-time",
      HiFunRand <$ string "rand",
      HiFunEcho <$ string "echo",
      HiFunCount <$ string "count",
      HiFunKeys <$ string "keys",
      HiFunValues <$ string "values",
      HiFunInvert <$ string "invert"
    ]

numberParser :: Parser HiValue
numberParser = HiValueNumber . toRational <$> L.signed space L.scientific

byteParser :: Parser Word8
byteParser = L.lexeme space L.hexadecimal

bytesParser :: Parser HiValue
bytesParser = HiValueBytes . B.pack <$> hashes (many byteParser)

boolParser :: Parser HiValue
boolParser = HiValueBool <$> (True <$ string "true" <|> False <$ string "false")

nullParser :: Parser HiValue
nullParser = HiValueNull <$ string "null"

valueFunParser :: Parser HiValue
valueFunParser = L.lexeme space $ HiValueFunction <$> funParser

valueParser :: Parser HiExpr
valueParser =
  L.lexeme space $
    HiExprValue
      <$> choice
        [ numberParser,
          valueFunParser,
          boolParser,
          nullParser,
          hiValStrParser,
          bytesParser,
          cwdParser,
          nowParser
        ]

cwdParser :: Parser HiValue
cwdParser = HiValueAction <$> (HiActionCwd <$ string "cwd")

nowParser :: Parser HiValue
nowParser = HiValueAction <$> (HiActionNow <$ string "now")

dictParser :: Parser HiExpr
dictParser = HiExprDict <$> braces (pairParser `sepBy` symbol ",")

pairParser :: Parser (HiExpr, HiExpr)
pairParser = do
  key <- operatorParser
  void $ symbol ":"
  val <- operatorParser
  return (key, val)

atomParser :: Parser HiExpr
atomParser = valueParser <|> listParser <|> dictParser <|> parens operatorParser

exprParser :: Parser HiExpr -- pTerm
exprParser = do
  fun <- atomParser
  args <- many $ parens argsParser <|> dotParser
  sign <- optional $ symbol "!"
  return $
    let app = foldl HiExprApply fun args
     in case sign of
          (Just _) -> HiExprRun app
          Nothing -> app

argsParser :: Parser [HiExpr]
argsParser = L.lexeme space $ operatorParser `sepBy` symbol ","

dotParser :: Parser [HiExpr]
dotParser = L.lexeme space $ (: []) . HiExprValue . HiValueString . T.pack <$> (symbol "." *> idParser)

idParser :: Parser [Char]
idParser = concat <$> (((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-')

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

hashes :: Parser a -> Parser a
hashes = between (symbol "[#") (symbol "#]")

listParser :: Parser HiExpr
listParser = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> brackets argsParser

termParser :: Parser HiExpr
termParser = exprParser <|> parens operatorParser

operators :: [[Operator Parser HiExpr]]
operators =
  [ [ InfixL $ binary HiFunMul <$ symbol "*",
      InfixL $ binary HiFunDiv <$ try (L.lexeme space (char '/' <* notFollowedBy (char '=')))
    ],
    [ InfixL $ binary HiFunAdd <$ symbol "+",
      InfixL $ binary HiFunSub <$ symbol "-"
    ],
    [ InfixN $ binary HiFunEquals <$ symbol "==",
      InfixN $ binary HiFunNotEquals <$ symbol "/=",
      InfixN $ binary HiFunNotLessThan <$ symbol ">=",
      InfixN $ binary HiFunNotGreaterThan <$ symbol "<=",
      InfixN $ binary HiFunLessThan <$ symbol "<",
      InfixN $ binary HiFunGreaterThan <$ symbol ">"
    ],
    [ InfixR $ binary HiFunAnd <$ symbol "&&"
    ],
    [ InfixR $ binary HiFunOr <$ symbol "||"
    ]
  ]

binary :: HiFun -> HiExpr -> HiExpr -> HiExpr
binary fun x y = HiExprApply (wrapIntoExpr fun) [x, y]

symbol :: String -> Parser String
symbol = L.symbol space

wrapIntoExpr :: HiFun -> HiExpr
wrapIntoExpr = HiExprValue . HiValueFunction

stringLiteral :: Parser T.Text
stringLiteral = T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

hiValStrParser :: Parser HiValue
hiValStrParser = HiValueString <$> stringLiteral

operatorParser :: Parser HiExpr
operatorParser = makeExprParser termParser operators

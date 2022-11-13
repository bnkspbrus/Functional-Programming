{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( ParseError (ErrorAtPos),
    runP,
    pChar,
    parseError,
    pEof,
    parseExpr,
  )
where

import Control.Applicative
import Control.Monad (MonadPlus, guard, mfilter, void)
import Data.Char
import GHC.Float.RealFracMethods (int2Double)
import GHC.Natural (Natural)
import HW2.T1 (Annotated ((:#)), Except (..), mapExcept)
import HW2.T4 (Expr (..))
import HW2.T5 (ExceptState (ES), runES)

data ParseError = ErrorAtPos Natural deriving (Show)

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P es) str = case runES es (0, str) of
  (Error e)          -> Error e
  (Success (a :# _)) -> Success a

--reads only one character from string. throws error when end of string is occurred. moves string position forward when read successful
pChar :: Parser Char
pChar = P $
  ES $ \(pos, s) ->
    case s of
      []       -> Error (ErrorAtPos pos)
      (c : cs) -> Success (c :# (pos + 1, cs))

peekChar :: Parser Char
peekChar = peekP pChar

peekP :: Parser a -> Parser a
peekP p = P $ ES $ \s@(_, str) -> mapExcept (:# s) (runP p str)

pInt :: Parser Int
pInt = do
  int <- some (mfilter isDigit pChar)
  return (parseInt int)

parseInt :: String -> Int
parseInt str = foldl (\acc x -> acc * 10 + x) 0 (map digitToInt str)

pFrac :: Parser Double
pFrac = do
  frac <- some (mfilter isDigit pChar)
  return $ let int = int2Double (parseInt frac) in foldr (\_ acc -> acc / 10) int frac

pNumber :: Parser Double
pNumber = do
  int <- pInt
  dot <- optional pChar
  case dot of
    (Just '.') -> do frac <- pFrac; return (int2Double int + frac)
    _          -> return (int2Double int)

parseError :: Parser a
parseError = P (ES $ \(pos, _) -> Error (ErrorAtPos pos))

instance Alternative Parser where
  empty = parseError
  P p <|> P q = P $
    ES $ \s ->
      case runES p s of
        (Success _) -> runES p s
        (Error _)   -> runES q s

instance MonadPlus Parser -- No methods.

pEof :: Parser ()
pEof = P $
  ES $ \(pos, s) ->
    case s of
      [] -> Success (() :# (pos, s))
      _  -> Error (ErrorAtPos pos)

-- E  -> TE'
-- E' -> +TE'
-- E' -> -TE'
-- E' -> empty
-- T  -> FT'
-- T' -> *FT'
-- T' -> /FT'
-- T' -> empty
-- F  -> (E)
-- F  -> n

pWhitespaces :: Parser ()
pWhitespaces = void (many (mfilter isSpace pChar))

data Token = PLUS | MINUS | STAR | SLASH | NUMBER | OBRACKET | CBRACKET | END deriving (Eq)

curToken :: Parser Token
curToken =
  do
    c <- optional peekChar
    case c of
      (Just v) ->
        case v of
          '+' -> return PLUS
          '-' -> return MINUS
          '*' -> return STAR
          '/' -> return SLASH
          '(' -> return OBRACKET
          ')' -> return CBRACKET
          _   -> if isDigit v then return NUMBER else parseError
      Nothing -> return END

nextToken :: Parser ()
nextToken = do skipCurToken; pWhitespaces

skipCurToken :: Parser ()
skipCurToken =
  do
    c <- optional peekChar
    case c of
      (Just v) ->
        if v `elem` ['+', '-', '*', '/', '(', ')']
          then void pChar
          else void pNumber
      Nothing -> return ()

parseE :: Parser Expr
parseE =
  do
    token <- curToken
    guard $ token `elem` [NUMBER, OBRACKET]
    t <- parseT
    parseE' t

parseT :: Parser Expr
parseT =
  do
    token <- curToken
    guard $ token `elem` [NUMBER, OBRACKET]
    f <- parseF
    parseT' f

parseE' :: Expr -> Parser Expr
parseE' left =
  do
    token <- curToken
    case token of
      PLUS     -> do nextToken; t <- parseT; e' <- parseE' t; return (left + e')
      MINUS    -> do nextToken; t <- parseT; e' <- parseE' t; return (left - e')
      END      -> return left
      CBRACKET -> return left
      _        -> parseError

parseF :: Parser Expr
parseF =
  do
    token <- curToken
    case token of
      NUMBER   -> do val <- peekP pNumber; nextToken; return (Val val)
      OBRACKET -> do nextToken; e <- parseE; nextToken; return e
      _        -> parseError

parseT' :: Expr -> Parser Expr
parseT' left =
  do
    token <- curToken
    case token of
      STAR     -> do nextToken; f <- parseF; t' <- parseT' f; return (left * t')
      SLASH    -> do nextToken; f <- parseF; t' <- parseT' f; return (left / t')
      END      -> return left
      CBRACKET -> return left
      PLUS     -> return left
      MINUS    -> return left
      _        -> parseError

parseExpr :: String -> Except ParseError Expr
parseExpr = runP $ do pWhitespaces; parseE

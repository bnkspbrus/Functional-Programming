{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6 where

import Control.Applicative
import Control.Monad (MonadPlus, guard, void)
import qualified Data.Char
import GHC.Natural
import HW2.T1 (Annotated ((:#)), Except (..))
import HW2.T4 (Expr (..))
import HW2.T5 (ExceptState (ES), runES)
import Data.Char (digitToInt)
import GHC.Float.RealFracMethods (int2Double)

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

len :: [a] -> Natural
len = foldr (\_ acc -> acc + 1) 0

pSkipWhitespaces :: Parser ()
pSkipWhitespaces = P $ ES $ \s -> Success (() :# skipWhitespaces s)

skipWhitespaces :: (Natural, String) -> (Natural, String)
skipWhitespaces (pos, str) = let (f, s) = span Data.Char.isSpace str in (pos + len f, s)

data Token = PLUS | MINUS | STAR | SLASH | NUMBER | OBRACKET | CBRACKET | END deriving (Eq)

curToken :: Parser Token
curToken =
  do
    pSkipWhitespaces
    P $ ES $ \s -> getToken s

getToken :: (Natural, String) -> Except ParseError (Annotated (Natural, String) Token)
getToken s@(pos, str)
  | null str = Success (END :# s)
  | otherwise = case head str of
    '+' -> Success (PLUS :# s)
    '-' -> Success (MINUS :# s)
    '*' -> Success (STAR :# s)
    '/' -> Success (SLASH :# s)
    '(' -> Success (OBRACKET :# s)
    ')' -> Success (CBRACKET :# s)
    _   -> if Data.Char.isDigit (head str) then Success (NUMBER :# s) else Error (ErrorAtPos pos)

nextToken :: Parser String
nextToken =
  do
    pSkipWhitespaces
    P $ ES getText

getText :: (Natural, String) -> Except ParseError (Annotated (Natural, String) String)
getText s@(pos, str)
  | null str = Success ("" :# s)
  | head str `elem` ['+', '-', '*', '/', '(', ')'] = Success ([head str] :# (pos + 1, tail str))
  | otherwise = getNumber s

getNumber :: (Natural, String) -> Except ParseError (Annotated (Natural, String) String)
getNumber (pos, str) =
  let (f, s) = span Data.Char.isDigit str
   in if null f
        then Error (ErrorAtPos pos)
        else
          if null s || head s /= '.'
            then Success (f :# (pos + len f, s))
            else
              let (f2, s2) = span Data.Char.isDigit (tail s)
               in if null f2
                    then Error (ErrorAtPos (pos + len f + 1))
                    else Success (f ++ "." ++ f2 :# (pos + len f + 1 + len f2, s2))

--getText = undefined

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
      PLUS     -> do void nextToken; t <- parseT; e' <- parseE' t; return (left + e')
      MINUS    -> do void nextToken; t <- parseT; e' <- parseE' t; return (left - e')
      END      -> return left
      CBRACKET -> return left
      _        -> parseError

parseDouble :: String -> Double
parseDouble str = let (f, s) = span Data.Char.isDigit str in let int = foldl (\acc x -> acc * 10 + x) 0 (map (int2Double . digitToInt) f) in
  if null s then int
  else let frac = foldr (\x acc -> (x + acc) / 10) 0 (map (int2Double . digitToInt) (tail s)) in int + frac

parseF :: Parser Expr
parseF =
  do
    token <- curToken
    case token of
      NUMBER   -> Val . parseDouble <$> nextToken
      OBRACKET -> do void nextToken; e <- parseE; void nextToken; return e
      _        -> parseError

parseT' :: Expr -> Parser Expr
parseT' left =
  do
    token <- curToken
    case token of
      STAR     -> do void nextToken; f <- parseF; t' <- parseT' f; return (left * t')
      SLASH    -> do void nextToken; f <- parseF; t' <- parseT' f; return (left / t')
      END      -> return left
      CBRACKET -> return left
      PLUS     -> return left
      MINUS    -> return left
      _        -> parseError

parseExpr :: String -> Except ParseError Expr
parseExpr = runP parseE

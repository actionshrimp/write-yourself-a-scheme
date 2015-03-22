module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.Char (digitToInt)
import Numeric (readInt, readOct, readHex, readDec)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
     deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapeChar :: Parser Char
escapeChar = do
  char '\\'
  x <- anyChar
  case x of
    't' -> return '\t'
    'n' -> return '\n'
    'r' -> return '\r'
    '\\' -> return '\\'
    '"' -> return '"'
    _ -> fail ("Unrecognised escape character:  \\" ++ [x])

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapeChar <|> noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ Atom (first:rest)

parseDigits :: String -> ReadS Integer -> Parser LispVal
parseDigits name reader = do
  ds <- many (letter <|> digit)
  let (p, f) = head . reader $ ds
  case f of
    "" -> return $ Number p
    _ -> fail ("Unrecognised " ++ name ++ " digits: " ++ f)

readBin :: (Integral a) => ReadS a
readBin = readInt 2 (\a -> a == '0' || a == '1') digitToInt

parseReader :: Parser LispVal
parseReader = do
  char '#'
  x <- letter
  case x of
    't' -> return $ Bool True
    'f' -> return $ Bool False
    'o' -> parseDigits "octal" readOct
    'x' -> parseDigits "hex" readHex
    'd' -> parseDigits "decimal" readDec
    'b' -> parseDigits "binary" readBin
    _ -> fail ("Unrecognised reader: " ++ [x])

parseNumber :: Parser LispVal
parseNumber = parseDigits "decimal" readDec

parseExpr :: Parser LispVal
parseExpr = parseReader <|> parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> show val

main :: IO ()
main = do
     args <- getArgs
     putStrLn (readExpr $ head args)

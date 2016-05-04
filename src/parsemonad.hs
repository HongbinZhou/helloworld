{-# LANGUAGE FlexibleContexts #-}
import Test.QuickCheck

import Control.Monad
import Control.Monad.Identity (Identity)
import System.Environment (getArgs)
import Text.Parsec hiding (spaces)

-- ref: https://en.wikibooks.org/wiki/Haskell/Practical_monads

symbol :: Stream s m Char => ParsecT s u m Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~" <?> "hi"

readExpr :: Stream s Identity Char => s -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Stream s m Char => ParsecT s u m ()
spaces = skipMany1 space

comp :: Stream s m Char => ParsecT s u m Char
comp = spaces >> symbol


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)

parseString :: Stream s m Char => ParsecT s u m LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Stream s m Char => ParsecT s u m LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

-- note: should use many1, rather than many
parseNumber :: Stream s m Char => ParsecT s u m LispVal
parseNumber = do x <- many1 digit
                 return $ Number (read x)

-- learn liftM!!!
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
parseNumber2 :: Stream s m Char => ParsecT s u m LispVal
parseNumber2 = liftM (Number . read)  (many1 digit)

parseExpr :: Stream s m Char => ParsecT s u m LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

parseList :: Stream s m Char => ParsecT s u m LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- try quickCheck
prop_reverse :: Eq a => [a] -> [a] -> Bool
prop_reverse x y = reverse (x++y) == (reverse y) ++ (reverse x)

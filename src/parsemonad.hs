{-# LANGUAGE FlexibleContexts #-}
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

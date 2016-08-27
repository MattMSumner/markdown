module Main where

import Control.Monad (void)
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Combinator
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

data MarkdownError = MarkdownError String deriving Show
data Block = Header Int deriving Show

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
    where lineCmnt  = L.skipLineComment "//"
          blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parseMarkdown :: Parser Block
parseMarkdown = do
    level <- count' 1 6 (symbol "#") >>= return . length
    return $ Header level

main :: IO ()
main = do
    (expr:_) <- getArgs
    parseTest parseMarkdown expr

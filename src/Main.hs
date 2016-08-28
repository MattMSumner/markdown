module Main where

import Control.Monad (void)
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Combinator
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

data MarkdownError = MarkdownError String deriving Show
data Block = Header Int String deriving Show

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
    where lineCmnt  = L.skipLineComment "//"
          blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parseMarkdown :: Parser [Block]
parseMarkdown = do
  parseBlocks <* eof

parseBlocks :: Parser [Block]
parseBlocks = manyTill block eof

block :: Parser Block
block = try $ do
    choice [ header ]

header :: Parser Block
header = do
    level <- count' 1 6 (symbol "#") >>= return . length
    text <- manyTill anyChar eol
    return $ Header level text

main :: IO ()
main = do
    expr <- getContents
    parseTest parseMarkdown expr

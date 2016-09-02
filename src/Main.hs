module Main where

import Control.Monad (void)
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Combinator
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

data Block = Header Int [Inline]
           | Punchline [Inline]
    deriving Show
data Inline = Strong String
            | Normy String
            | Muggle String
            | Recall [Inline] String
            | Space
    deriving Show

sc :: Parser ()
sc = void spaceChar

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseMarkdown :: Parser [Block]
parseMarkdown = do
  parseBlocks

parseBlocks :: Parser [Block]
parseBlocks = manyTill block eof

parseInline :: Parser [Inline]
parseInline = manyTill inline eol

inline :: Parser Inline
inline = try $ do
    choice [ spaceman
           , strong
           , normies
           , plainOldRecall
           , muggle
           ]

strong :: Parser Inline
strong = try $ do
    text <- string "**" *> manyTill anyChar (string "**")
    return $ Strong text

normies :: Parser Inline
normies = try $ do
    text <- string "*" *> manyTill anyChar (string "**")
    return $ Normy text

spaceman :: Parser Inline
spaceman = try $ do
    space <- spaceChar
    return Space

plainOldRecall :: Parser Inline
plainOldRecall = try $ do
    text <- (between (string "[") (string "]") parseInline)
    url <- (between (string "(") (string ")") (some (noneOf ")")))
    return $ Recall text url

muggle :: Parser Inline
muggle = try $ do
    text <- someTill anyChar (lookAhead sc)
    return $ Muggle text

block :: Parser Block
block = try $ do
    choice [ header ]

header :: Parser Block
header = try $ do
    choice [ inlineHeader, equalBlockHeader, dashyBlockHeader ]

equalBlockHeader :: Parser Block
equalBlockHeader = try $ do
    text <- parseInline
    someTill (string "=") eol
    lexeme $ return $ Header 1 text

dashyBlockHeader :: Parser Block
dashyBlockHeader = try $ do
    text <- parseInline
    someTill (string "-") eol
    lexeme $ return $ Header 2 text

inlineHeader :: Parser Block
inlineHeader = try $ do
    level <- count' 1 6 (string "#") >>= return . length
    text <- (string " ") *> parseInline
    lexeme $ return $ Header level text

main :: IO ()
main = do
    expr <- getContents
    parseTest parseMarkdown expr

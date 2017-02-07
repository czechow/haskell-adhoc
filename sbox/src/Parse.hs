module Parse where

import Text.ParserCombinators.Parsec
import Text.Parsec (Parsec)


data Paragraph = Paragraph String String
               deriving (Show)


paragraph :: Parser Paragraph
paragraph = do
  _ <- many (many (oneOf " \t") <* string "\n")
  ll <- livyLine
  ls <- manyTill anyChar (try (lookAhead (livyLine <|> (do eof; return ""))))
  return $ Paragraph ll ls

paragraphs :: Parser [Paragraph]
paragraphs = many paragraph <* eof

-- A bit clumsy here...
livyLine :: Parser String
livyLine = do
      string "%livy"
   <* many (oneOf " \t")
   <* string "\n"

line :: Parser Char
line = char 'x'

go :: (Show a) => Parsec String () a -> String -> IO ()
go p s = parseTest p s

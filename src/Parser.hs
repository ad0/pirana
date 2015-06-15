module Parser (parsePiProcess) where

import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Prim

import Control.Monad
import Control.Arrow

import Syntax

inertParser :: Parser (Process String)
inertParser = char '0' >> return Inert
          <?> "inert"

nuParser :: Parser (Process String)
nuParser = liftM (uncurry Local) $ allocParser (string "new")

localParser :: Parser (Process String)
localParser = liftM (uncurry Local) $ allocParser (string "local")

allocParser :: Parser a -> Parser (String, Process String)
allocParser pref = do
                     _ <- pref
                     spaces
                     n <- parens $ identifier lower
                     p <- contParser
                     return (n,p)

tauParser :: Parser (Process String)
tauParser = do
              _ <- string "tau"
              liftM TauPref contParser
              <?> "tau"

inParser :: Parser (Process String)
inParser = do
             c <- identifier lower
             _ <- char '?'
             d <- parens $ identifier lower
             p <- contParser
             return $ InPref c d p
             <?> "input"

outParser :: Parser (Process String)
outParser = do
              c <- identifier lower
              _ <- char '!'
              x <- identifier lower
              p <- contParser
              return $ OutPref c x p
              <?> "output"

contParser :: Parser (Process String)
contParser = do
               _ <- char '.'
               spaces
               processParser
               <?> "continuation"

callParser :: Parser (Process String)
callParser = do
               defnam <- identifier upper
               params <- brackets $ identifier lower `sepBy` comma
               return $ Call defnam params
               <?> "call"

infixBinParser :: Parser a -> Parser (Process String, Process String)
infixBinParser sep = do
                       spaces
                       p <- processParser
                       spaces
                       _ <- sep
                       spaces
                       q <- processParser
                       spaces
                       return (p,q)

parParser :: Parser (Process String)
parParser = liftM (uncurry Par) $ infixBinParser (char '|')

sumParser :: Parser (Process String)
sumParser = liftM (uncurry Sum) $ infixBinParser (char '+')

processParser :: Parser (Process String)
processParser = try inertParser
            <|> try nuParser
            <|> try localParser
            <|> try tauParser
            <|> try inParser
            <|> try outParser
            <|> callParser
            <|> try (parens parParser)
            <|> parens sumParser
            <?> "process parser"

defParser :: Parser (Def String)
defParser = do
              _      <- string "def"
              spaces
              defnam <- identifier upper
              params <- parens $ identifier lower `sepBy` comma
              spaces
              _      <- char '='
              spaces
              body   <- processParser
              spaces
              return $ Def defnam params body

identifier :: Parser Char -> Parser String
identifier f = do
                 x  <- f
                 xs <- many (alphaNum <|> char '_')
                 return $ x:xs

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

comma :: Parser ()
comma = spaces >> char ',' >> spaces

piFile :: Parser ([Def String], Process String)
piFile = do
  defs <- many defParser
  proc <- processParser
  return (defs, proc)

parsePiProcess :: String -> Either String ([Def String], Process String)
parsePiProcess s = left show $ parse piFile "PiFile Parser" s

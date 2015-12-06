module Language.FOmega.Parse where

import Language.FOmega.Syntax (Stmt(..), Tm(..), Pgm(..), pgm2Tm)

import Prelude hiding (pi)

import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.String (parseFromFile)
import Text.Parsec.Language 

parseExpr = parse expr ""
parseStmt = parse stmt ""
parsePgm  = parse pgm ""

pgm   = Pgm <$> stmts <*> expr

stmts = Stmts <$> many stmt
stmt  = decl <|> load

parseEnv :: [(String,String)] -> Either ParseError [(String,Tm ())]
parseEnv = mapM go
  where go (x,y) = do y' <- parseExpr y
                      return (x,y')

builtinFrom :: [String] -> String -> IO (Tm ())
builtinFrom libs src = do
  let loads = concat $ map (\l -> "%load \"" ++ l ++ "\"; ") libs
  pgm <- loadPgmSrc $ loads ++ src
  return $ pgm2Tm pgm

builtin = builtinFrom ["Repr"] 

loadPgmSrc src = do
  let Right pgm = parsePgm src
  loadPgm "fomega/lib" pgm

-- recursively parse loads
loadPgm libDir (Pgm stmt tm) = Pgm <$> doLoads libDir stmt <*> pure tm

loadPgmFile libDir file = do
  p <- parseFromFile pgm file
  case p of
    Left e     -> fail (show e)
    Right p    -> loadPgm libDir p

loadStmts libDir file = do
  p <- parseFromFile stmts file
  case p of
    Left e   -> fail (show e)
    Right s  -> doLoads libDir s

doLoads dir = go
  where go (Stmts stmts) = Stmts <$> mapM go stmts
        go (SLoad s)     = loadStmts dir (dir ++ "/" ++ s ++ ".fw")
        go s             = return s

decl  = do try $ symbol "%decl"
           nm <- identifier
           symbol ":"
           ty <- expr
           symbol "="
           def <- expr
           symbol ";"
           return $ SDecl nm ty def

load = do try $ symbol "%load"
          nm <- stringLiteral
          symbol ";"
          return $ SLoad nm

expr  =   lambda
      <|> pi
      <|> arr

quotation = Quote () <$> brackets expr

app = do parts <- many (var <|> sort <|> quotation <|> parens expr)
         if null parts 
            then fail "Couldn't parse application"
            else return $ foldl1 (App ()) parts
arr = chainr1 (pi <|> app) (symbol "->" >> return (Pi () ""))

lambda = do symbol "\\"
            x <- identifier
            symbol ":"
            t <- expr
            symbol "."
            b <- expr
            return $ Lam () x t b

pi = do try $ symbol "Pi"
        x <- identifier
        symbol ":"
        t <- expr
        symbol "."
        b <- expr
        return $ Pi () x t b

  
var = Var () <$> identifier
sort = star <|> box -- <|> delta

-- The lexer
langDef = haskellDef { 
    P.reservedNames = ["Pi"]
  , P.identStart = letter <|> char '_'
  , P.identLetter = alphaNum <|> oneOf "_*"
  }
lexer       = P.makeTokenParser langDef
     
parens        = P.parens lexer
brackets      = P.brackets lexer
identifier    = P.identifier lexer
reserved      = P.reserved lexer
symbol        = P.symbol lexer
stringLiteral = P.stringLiteral lexer

star  = symbol "*" >> return (Star ())
box   = symbol "#" >> return (Box ())
--delta = symbol "^" >> return Delta


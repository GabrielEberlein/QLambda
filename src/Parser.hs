{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where

import           Prelude hiding (const, abs)
import           Text.Parsec hiding (runP)
import           Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex
import           Text.Parsec.Language
import Control.Monad.Identity (Identity)

import           AST

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser langDef

langDef :: LanguageDef u
langDef = emptyDef {
         Tok.commentLine    = "#",
         Tok.reservedNames = ["def","if", "then", "else", "match", "with",
                              "let", "rec", "in",
                              "injl", "injr", 
                              "new", "meas",
                              "X", "Y", "Z", "H", "CNOT", "0", "1","*"],
         Tok.reservedOpNames = ["->","\\",".","=","|"]
        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer
natural = Tok.natural lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

angles :: P a -> P a
angles = Tok.angles lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

tyIdentifier :: P String
tyIdentifier = Tok.lexeme lexer $ do
  c  <- upper
  cs <- many (identLetter langDef)
  return (c:cs)

-----------------------
-- Parsers
-----------------------

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier

pair :: P [Name]
pair = angles $ do l <- var
                   reservedOp ","
                   r <- var
                   return [l,r]

gate :: P Gate
gate =     (reserved "X" >> return X)
       <|> (reserved "Y" >> return Y)
       <|> (reserved "Z" >> return Z)
       <|> (reserved "H" >> return H)
       <|> (reserved "CNOT" >> return CNot)

const :: P Const
const =     U <$> gate
        <|> (reserved "new" >> return New)
        <|> (reserved "meas" >> return Meas)

tuple :: P STerm
tuple = do ts <- angles $ tm `sepBy` reservedOp ","
           return $ STuple ts

inj :: P STerm
inj = do reserved "injl"
         t <- parens tm
         return $ SInjL t
  <|> do reserved "injr"
         t <- parens tm
         return $ SInjR t

atom :: P STerm
atom =      SC <$> const
       <|>  SV <$> var
       <|>  tuple
       <|>  inj
       <|> (reserved "0" >> return SZero)
       <|> (reserved "1" >> return SOne)
       <|> (reserved "*" >> return (STuple []))
       <|>  parens tm

absVars :: P ([Name], AbsType)
absVars = do vars <- many var
             return (vars, AVar)

absPair :: P ([Name], AbsType)
absPair = do p <- pair
             return (p, APair)

abs :: P STerm
abs = do reservedOp "\\"
         (vars, absTy) <- try absPair <|> absVars
         reservedOp "."
         t <- tm
         return (SAbs absTy vars t)

-- Nota el parser app también parsea un solo atom.
app :: P STerm
app = do f <- atom
         args <- many atom
         return (foldl SApp f args)

ifexp :: P STerm
ifexp = do reserved "if"
           c <- tm
           reserved "then"
           t <- tm
           reserved "else"
           e <- tm
           return (SIf c t e)

-- letRec :: P ([Name], LetType)
-- letRec = do reserved "rec"
--             f <- var
--             vars <- many var
--             reservedOp "="
--             return (f:vars, LRec)

letFun :: P ([Name], LetType)
letFun = do f <- var
            vars <- many var
            reservedOp "="
            return (f:vars, LFun)

letPair :: P ([Name], LetType)
letPair = do p <- pair
             reservedOp "="
             return (p, LPair)

letVar :: P ([Name], LetType)
letVar = do v <- var
            reservedOp "="
            return ([v], LVar)

letexp :: P STerm
letexp = do reserved "let" 
            (vars, letTy) <- try letVar <|> try letPair <|> letFun
            t <- tm
            reserved "in"
            t' <- tm
            return (SLet letTy vars t t')

match :: P STerm
match = do reserved "match"
           t <- tm
           reserved "with"
           parens $ do x <- var
                       reservedOp "->"
                       t1 <- tm
                       reserved "|"
                       y <- var
                       reservedOp "->"
                       t2 <- tm
                       return $ SMatch t x t1 y t2

-- | Parser de términos
tm :: P STerm
tm = abs <|> letexp <|> app <|> atom <|> match <|> ifexp


-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
stmt :: P (Stmt STerm)
stmt =  do reserved "def"
           n <- var
           reservedOp "="
           t <- tm
           return $ Def n t

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> Stmt STerm
parse s = case runP stmt s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)

program :: P [Stmt STerm]
program = many stmt

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Parser where

import           Prelude hiding (const, abs)
import           Text.Parsec hiding (runP)
import           Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Token as Tok

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
         Tok.identStart    = letter <|> char '_',
         Tok.opStart       = oneOf "=.\\",
         Tok.opLetter      = parserZero,
         Tok.commentLine   = "#",
         Tok.reservedNames = ["def","if", "then", "else", "match", "with",
                              "let", "rec", "in",
                              "injl", "injr", 
                              "new", "meas",
                              "printState",
                              "X", "Y", "Z", "H", "CNOT", "0", "1","*"],
         Tok.reservedOpNames = ["->","\\",".","=","|",","]
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

printexp :: P STerm
printexp = do reserved "printState"
              s <- stringLiteral
              t <- tm
              return $ SPrint s t

atom :: P STerm
atom =      SC <$> const
       <|>  SV <$> var
       <|>  tuple
       <|>  inj
       <|>  printexp
       <|> (reserved "0" >> return SZero)
       <|> (reserved "1" >> return SOne)
       <|> (reserved "*" >> return (STuple []))
       <|>  parens tm

absNull :: P ([Name], AbsType)
absNull = do reserved "*"
             reservedOp "."
             return ([], ANull)

absVars :: P ([Name], AbsType)
absVars = do vars <- many var
             reservedOp "."
             return (vars, AVar)

pair :: P [Name]
pair = angles $ do l <- var
                   reservedOp ","
                   r <- var
                   return [l,r]

absPair :: P ([Name], AbsType)
absPair = do p <- pair
             reservedOp "."
             return (p, APair)

abs :: P STerm
abs = do reservedOp "\\"
         (vars, absTy) <- try absPair <|> try absVars <|> absNull
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

letNull :: P ([Name], LetType)
letNull = do reserved "*"
             reservedOp "="
             return ([], LNull)

letRec :: P ([Name], LetType)
letRec = do reserved "rec"
            f <- var
            vars <- many var
            reservedOp "="
            return (f:vars, LRec)

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
            (vars, letTy) <- try letVar <|> try letPair <|> try letFun <|> try letRec <|> letNull
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
decl :: P (Decl STerm)
decl =  do reserved "def"
           n <- var
           reservedOp "="
           t <- tm
           return $ Def n t

declOrTm :: P (Either (Decl STerm) STerm)
declOrTm =  (Left <$> decl) <|> (Right <$> tm)

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> Decl STerm
parse s = case runP decl s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s ++ "\n error: " ++ show e)

program :: P [Decl STerm]
program = many decl
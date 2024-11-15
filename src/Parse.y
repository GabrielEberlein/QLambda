{
module Parse where
import AST
import Data.Maybe
import Data.Char

}

%monad { P } { thenP } { returnP }
%name parseStmt Stmt
%name parseStmts Stmts
%name term Exp

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '='     { TEquals }
    '\\'    { TAbs }
    '.'     { TDot }
    ','     { TComa }
    '('     { TOpen }
    ')'     { TClose }
    '->'    { TArrow }
    '|'     { TPipe}
    '*'     { TOple }    
    '1'     { TOne }
    '0'     { TZero }
    'X'     { TX }
    'Y'     { TY }
    'Z'     { TZ }
    'H'     { TH }
    CNOT    { TCnot }
    VAR     { TVar $$ }
    INJL    { TInjL }
    INJR    { TInjR }
    NEW     { TNew }
    MEAS    { TMeas }
    DEF     { TDef }
    LET     { TLet }
    IN      { TIn }
    MATCH   { TMatch }
    WITH    { TWith }

%right VAR
%left '=' 
%right '->'
%right '\\' '.' LET IN MATCH WITH
-- %right REC 
%right INJL INJR

%%

Stmt    :  Def                         { $1 }
        |  Exp	                       { Eval $1 }

Def     : DEF VAR '=' '(' Exp ')'      { Def $2 $5 } 

Exp     :: { Term }
        : '\\' VAR '.' Exp    { Abs $2 $4 }
        | LET '(' VAR ',' VAR ')' '=' Exp IN Exp { Let $3 $5 $8 $10 }
        -- | REC Atom Atom Exp            { LRec $2 $3 $4}
        | MATCH Exp WITH '(' VAR '->' Exp '|' VAR '->' Exp ')' {Match $2 $5 $7 $9 $11}
        | INJL Exp                     { InjL $2 }
        | INJR Exp                     { InjR $2 }
        | Abs                         { $1 }

Abs    :: { Term }
        : Abs Atom                    { App $1 $2 }
        | Atom                         { $1 }

Atom    :: { Term }
        : VAR                          { Var $1 }
        | Const                        { C $1 }
        | '*'                          { Ople }
        | '1'                          { InjL Ople }
        | '0'                          { InjR Ople }
        | '(' Exp ',' Exp ')'          { Pair $2 $4 }
        | '(' Exp ')'                  { $2 }

Const  :: { Const }
        : NEW                        { New }      
        | MEAS                       { Meas }
        | Gate                       { U $1 }

Gate    :: { Gate }
        : 'X'                            { X }
        | 'Y'                            { Y }
        | 'Z'                            { Z }
        | 'H'                            { H }
        | CNOT                           { CNot }

-- Type    : TYPEE                        { EmptyT }
--         | TYPEU                        { UnitT }
--         | TYPEN                        { NatT }
--         | Type '->' Type               { FunT $1 $3 }
--         | '(' Type ',' Type ')'        { PairT $2 $4 }
--         | '(' Type ')'                 { $2 }

Stmts    : Stmt Stmts                  { $1 : $2 }
        |                              { [] }
     
{

data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TVar String
                | TInjL
                | TInjR
                | TNew
                | TMeas
                | TCnot
                | TX
                | TY
                | TZ
                | TH
                | TOple
                | TPipe
                | TMatch
                | TWith
                | TOne
                | TZero
                | TDef
                | TAbs
                | TLet
                | TIn
                | TDot
                | TComa
                | TOpen
                | TClose
                | TArrow
                | TEquals
                | TEOF
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs	
                    ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TArrow cs
                    ('\\':cs)-> cont TAbs cs
                    ('.':cs) -> cont TDot cs
                    (',':cs) -> cont TComa cs
                    ('(':cs) -> cont TOpen cs
                    ('-':('>':cs)) -> cont TArrow cs
                    (')':cs) -> cont TClose cs
                    ('=':cs) -> cont TEquals cs
                    ('1':cs) -> cont TOne cs
                    ('0':cs) -> cont TZero cs
                    ('|':cs) -> cont TPipe cs
                    ('*':cs) -> cont TOple cs
                    unknown -> \line -> Failed $ 
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlpha cs of
                              ("CNOT",rest) -> cont TCnot rest
                              ("X",rest) -> cont TX rest
                              ("Y",rest) -> cont TY rest
                              ("Z",rest) -> cont TZ rest
                              ("H",rest) -> cont TH rest
                              ("new",rest)  -> cont TNew rest
                              ("meas",rest) -> cont TMeas rest
                              ("injl",rest) -> cont TInjL rest
                              ("injr",rest) -> cont TInjR rest
                              ("def",rest)  -> cont TDef rest
                              ("let",rest)  -> cont TLet rest
                              ("in",rest)   -> cont TIn rest
                              ("match",rest) -> cont TMatch rest
                              ("with",rest) -> cont TWith rest
                              (var,rest)    -> cont (TVar var) rest
                          consumirBK anidado cl cont s = case s of
                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
                              ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> consumirBK (anidado-1) cl cont cs
                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                              (_:cs) -> consumirBK anidado cl cont cs     
                                           
stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1
term_parse s = term s 1
}

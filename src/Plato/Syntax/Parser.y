{
module Plato.Syntax.Parser where

import Plato.Common.Name as N
import Plato.Common.Info
import Plato.Syntax.Lexer
import qualified Plato.Syntax.Abstract as A
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexer } { TokEof }
%error { parseError }

%token

'case'                          { TokKeyword (KwCase, $$) }
'data'                          { TokKeyword (KwData, $$) }
'forall'                        { TokKeyword (KwForall, $$) }
'in'                            { TokKeyword (KwIn, _) }
'of'                            { TokKeyword (KwOf, _) }
'let'                           { TokKeyword (KwLet, $$) }
'type'                          { TokKeyword (KwType, $$) }
'where'                         { TokKeyword (KwWhere, _) }

'->'                            { TokSymbol (SymArrow, $$) }
'\\'                            { TokSymbol (SymBackslash, $$)}
','                             { TokSymbol (SymComma, _) }
':'                             { TokSymbol (SymColon, _) }
'.'                             { TokSymbol (SymDot, $$) }
'='                             { TokSymbol (SymEqual, _) }
'{'                             { TokSymbol (SymLBrace, _) }
'('                             { TokSymbol (SymLParen, _) }
'}'                             { TokSymbol (SymRBrace, _) }
')'                             { TokSymbol (SymRParen, _) }
';'                             { TokSymbol (SymSemicolon, _) }
'_'                             { TokSymbol (SymUScore, $$) }
'|'                             { TokSymbol (SymVBar, _) }

varid                           { TokVarId $$ }
conid                           { TokConId $$ }
varsym                          { TokVarSym $$ }

float                           { TokFloat $$ }
string                          { TokString $$ }

%%

program     : topdecls                      { $1 }

topdecls    :: { [A.TopDecl] }
            : topdecl ';' topdecls          { $1 : $3 }
            | {- empty -}                   { [] }

topdecl     :: { A.TopDecl }
            : 'data' conid tyargs '=' constrs       { A.DataDecl (mkInfo $1) (id2name $2) $3 $5 }
            | 'type' conid  tyargs'=' type          { A.TypeDecl (mkInfo $1) (id2name $2) $3 $5 }
            | decl                                  { A.Decl $1 }

decls       :: { [A.Decl] }
            : decl ';'                      { [$1] }
            | decl ';' decls                { $1 : $3 }

decl        :: { A.Decl }
            : varid ':' type                { A.FuncTyDecl (mkInfo $1) (id2name $1) $3 }
            | varid '=' expr                { A.FuncDecl (mkInfo $1) (id2name $1) $3 }

types       :: { [A.Type] }
            : btype types                   { $1 : $2 }
            | {- empty -}                   { [] }

type        :: { A.Type }
            : btype '->' type               { A.ArrType (mkInfo $2) $1 $3 }
            | 'forall' vars '.' type        { A.AllType (mkInfo $1) $2 $4 }
            | btype                         { $1 }

btype       :: { A.Type }
            : btype atype                   { A.AppType $1 $2 }
            | '(' type ')'                  { $2 }
            | atype                         { $1 }

atype       :: { A.Type }
            : conid                         { A.ConType (mkInfo $1) (id2name $1) }
            | varid                         { A.VarType (mkInfo $1) (id2name $1) }

constrs     :: { [(N.Name, [A.Type])] }
            : constr '|' constrs            { $1 : $3 }
            | constr                        { [$1] }

constr      :: { (N.Name, [A.Type]) }
            : conid types                   { (id2name $1, $2) }

tyargs      :: { [N.Name] }
            : varid tyargs                  { id2name $1 : $2 }
            | {- empty -}                   { [] }

expr        :: { A.Expr }
            : varid args                                { A.VarExpr (mkInfo $1) (id2name $1) $2 }
            | conid args                                { A.ConExpr (mkInfo $1) (id2name $1) $2 }
            | float                                     { A.FloatExpr (fst $1) }
            | string                                    { A.StringExpr (fst $1) }
            | '(' expr ')'                              { $2 }
            | '\\' vars '->' expr                       { A.LamExpr (mkInfo $1) $2 $4 }
            | 'let' '{' decls '}' 'in' expr             { A.LetExpr (mkInfo $1) $3 $6 }
            | 'case' expr 'of' '{' alts '}'             { A.CaseExpr (mkInfo $1) $2 $5 }

args        :: { [A.Expr] }
            : '(' expr ')' args             { $2 : $4 }
            | varid args                    { A.VarExpr (mkInfo $1) (id2name $1) [] : $2 }
            | conid args                    { A.ConExpr (mkInfo $1) (id2name $1) [] : $2 }
            | float args                    { A.FloatExpr (fst $1) : $2 }
            | string args                   { A.StringExpr (fst $1) : $2 }
            | {- empty -}                   { [] }

vars        :: { [N.Name] }
            : varid vars                    { id2name $1 : $2 }
            | varid                         { [id2name $1] }

alts        :: { [(A.Expr, A.Expr, Info)] }
            : alt ';' alts                  { $1 : $3 }
            | {- empty -}                   { [] }

alt         :: { (A.Expr, A.Expr, Info) }
            : pat '->' expr                 { ($1, $3, mkInfo $2) }

pat         :: { A.Expr }
            : expr                          { $1 }
            | '_'                           { wildcard $1 }

{
parseError :: Token -> Alex a
parseError t = alexError $ "parse error: " ++ prettyToken t
    {-where
        (_, p) = t
        l = line $ pos p
        llen = length l
        offset = replicate (llen + 1) " "
        out = offset ++ "|\n" ++ l ++ " |" ++ prettyToken ++ "\n" ++ offset ++ " |\n"-}

id2name :: (String,  AlexPosn) -> Name
id2name = N.str2name . fst

wildcard :: AlexPosn -> A.Expr
wildcard p = A.ConExpr (mkInfo p) (N.str2name "_") []

class MkInfo a where
    mkInfo :: a -> Info

instance MkInfo AlexPosn where
    mkInfo (AlexPn _ l c) = Info{ line = l, col = c }

instance MkInfo (String, AlexPosn) where
    mkInfo (_, p) = mkInfo p

prettyToken :: Token -> String
prettyToken (TokKeyword (k, p)) = "'" ++ case k of
    KwCase -> "case"
    KwData -> "data"
    KwOf -> "of"
    KwLet -> "let"
    KwType -> "type"
    KwWhere -> "where"
    ++ "' at " ++ prettyPos p
prettyToken (TokSymbol (s, p)) = "'" ++ case s of
    SymArrow -> "->"
    SymBackslash -> "\\"
    SymColon -> ":"
    SymComma -> ","
    SymDot -> "."
    SymEqual -> "="
    SymLBrace -> "{"
    SymLParen -> "("
    SymRBrace -> "{"
    SymRParen -> ")"
    SymSemicolon -> ";"
    SymVBar -> "|"
    ++ "' at " ++ prettyPos p
prettyToken (TokVarId (s, p)) = "'" ++ s ++ "' at " ++ prettyPos p
prettyToken (TokConId (s, p)) = "'" ++ s ++ "' at " ++ prettyPos p
prettyToken (TokVarSym (s, p)) = "'" ++ s ++ "' at " ++ prettyPos p
prettyToken (TokFloat (i, p)) = show i ++ " at " ++ prettyPos p
prettyToken (TokString (s, p)) = "\"" ++ s ++ "\" at " ++ prettyPos p
prettyToken TokEof = ""

prettyPos :: AlexPosn -> String
prettyPos (AlexPn _ l c) = show l ++ ":" ++ show c
}

{
module Plato.Syntax.Parser where

import Plato.Common.Name as N
import Plato.Common.Position
import Plato.Syntax.Lexer
import qualified Plato.Syntax.AST as A
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexer } { TokEof }
%error { parseError }

%token

'case'                          { TokKeyword (KwLet, $$) }
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
            : 'data' conid tyargs '=' constrs       { A.DataDecl (id2name $2) $3 $5 (pos $1) }
            | 'type' conid  tyargs'=' type          { A.TypeDecl (id2name $2) $3 $5 (pos $1) }
            | decl                                  { A.Decl $1 }

decls       :: { [A.Decl] }
            : decl ';' decls                { $1 : $3 }
            | decl                          { [$1] }

decl        :: { A.Decl }
            : varid ':' type                { A.FuncTyDecl (id2name $1) $3 (pos $1) }
            | varid '=' expr                { A.FuncDecl (id2name $1) $3 (pos $1) }

types       :: { [A.Type] }
            : type types                    { $1 : $2 }
            | {- empty -}                   { [] }

type        :: { A.Type }
            : btype '->' type               { A.FunType $1 $3 (pos $2) }
            | 'forall' varid '.' type       { A.AllType (id2name $2) $4}
            | btype                         { $1 }

btype       :: { A.Type }
            : btype atype                   { A.AppType $1 $2 }
            | '(' type ')'                  { $2 }
            | atype                         { $1 }

atype       :: { A.Type }
            : conid                         { A.ConType (id2name $1) (pos $1) }
            | varid                         { A.VarType (id2name $1) (pos $1) }

constrs     :: { [(N.Name, [A.Type])] }
            : constr '|' constrs            { $1 : $3 }
            | constr                        { [$1] }

constr      :: { (N.Name, [A.Type]) }
            : conid types                   { (id2name $1, $2) }

tyargs      :: { [N.Name] }
            : varid tyargs                  { id2name $1 : $2 }
            | {- empty -}                   { [] }

expr        :: { A.Expr }
            : varid                                     { A.VarExpr (id2name $1) (pos $1) }
            | float                                     { A.FloatExpr (fst $1) }
            | string                                    { A.StringExpr (fst $1) }
            | varid args                                { A.CallExpr (id2name $1) $2 (pos $1) }
            | '(' expr ')'                              { $2 }
            | '\\' varid '->' expr                      { A.LamExpr (id2name $2) $4 (pos $1) }
            | 'let' '{' decls '}' 'in' expr             { A.LetExpr $3 $6 (pos $1) }
            | 'case' expr 'of' '{' pats '}'             { A.CaseExpr $2 $5 (pos $1) }

args        :: { [A.Expr] }
            : expr args                     { $1 : $2 }
            | {- empty -}                   { [] }

pats        :: { [(A.Expr, A.Expr, Pos)] }
            : expr '->' expr pats           { ($1, $3, pos $2) : $4 }
            | {- empty -}                   { [] }


{
parseError :: Token -> Alex a
parseError t = alexError $ "parse error: " ++ prettyToken t

id2name :: (String,  AlexPosn) -> Name
id2name = N.str2name . fst

class GetPos a where
    pos :: a -> Pos

instance GetPos AlexPosn where
    pos (AlexPn _ l c) = Pos l c

instance GetPos (String, AlexPosn) where
    pos (_, p) = pos p

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

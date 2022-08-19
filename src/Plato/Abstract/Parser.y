{
module Plato.Abstract.Parser where

import Plato.Common.Name as N
import Plato.Common.Info
import Plato.Abstract.Lexer
import qualified Plato.Abstract.Syntax as A
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
'import'                        { TokKeyword (KwImport, _) }
'in'                            { TokKeyword (KwIn, _) }
'of'                            { TokKeyword (KwOf, _) }
'module'                        { TokKeyword (KwModule, _) }
'let'                           { TokKeyword (KwLet, $$) }
'where'                         { TokKeyword (KwWhere, _) }

'\''                            { TokSymbol (SymApost, _) }
'->'                            { TokSymbol (SymArrow, $$) }
'\\'                            { TokSymbol (SymBackslash, $$)}
','                             { TokSymbol (SymComma, _) }
':'                             { TokSymbol (SymColon, _) }
'.'                             { TokSymbol (SymDot, $$) }
'='                             { TokSymbol (SymEqual, _) }
'{'                             { TokSymbol (SymLBrace, _) }
'['                             { TokSymbol (SymLBrack, $$) }
'('                             { TokSymbol (SymLParen, _) }
'}'                             { TokSymbol (SymRBrace, _) }
']'                             { TokSymbol (SymRBrack, _) }
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

program     : 'module' modid ';' body       { A.Program (Just $2) (fst $4) (snd $4) }
            | body                          { A.Program Nothing (fst $1) (snd $1) }

body        : impdecls ';' topdecls         { ($1, $3) }
            | topdecls                      { ([], $1) }

impdecls    :: { [A.ImpDecl] }
            : impdecl ';' impdecls          { $1 : $3 }
            | {- empty -}                   { [] }

impdecl     :: { A.ImpDecl }
            : 'import' modid                { A.ImpDecl $2 }

modid       :: { N.ModuleName }
            : modid_                        { N.ModuleName $1 }

modid_      :: { [N.Name] }
            : conid '.' modid_              { id2conName $1 : $3 }
            | conid                         { [id2conName $1] }

topdecls    :: { [A.TopDecl] }
            : topdecl ';' topdecls          { $1 : $3 }
            | {- empty -}                   { [] }

topdecl     :: { A.TopDecl }
            : 'data' conid tyvars '=' constrs       { A.DataDecl (mkInfo $1) (id2tyConName $2) $3 $5 }
            | 'data' conid tyvars                   { A.DataDecl (mkInfo $1) (id2tyConName $2) $3 [] }
            | conid tyvars '=' type                 { A.TypeDecl (mkInfo $1) (id2tyConName $1) $2 $4 }
            | decl                                  { A.Decl $1 }

decls       :: { [A.Decl] }
            : decl ';'                      { [$1] }
            | decl ';' decls                { $1 : $3 }

decl        :: { A.Decl }
            : varid ':' type                { A.FuncTyDecl (mkInfo $1) (id2varName $1) $3 }
            | varid vars '=' expr           { A.FuncDecl (mkInfo $1) (id2varName $1) $2 $4 }

types       :: { [A.Type] }
            : atype types                   { $1 : $2 }
            | {- empty -}                   { [] }

type        :: { A.Type }
            : 'forall' tyvars '.' type      { A.AllType (mkInfo $1) $2 $4 }
            | btype '->' type               { A.ArrType (mkInfo $2) $1 $3 }
            | '(' type ')'                  { $2 }
            | btype                         { $1 }

btype       :: { A.Type }
            : btype atype                   { A.AppType $1 $2 }
            | atype                         { $1 }

atype       :: { A.Type }
            : '(' type ')'                  { $2 }
            | conid                         { A.ConType (mkInfo $1) (id2tyConName $1) }
            | varid                         { A.VarType (mkInfo $1) (id2tyVarName $1) }

constrs     :: { [(Info, N.Name, [A.Type])] }
            : constr '|' constrs            { $1 : $3 }
            | constr                        { [$1] }

constr      :: { (Info, N.Name, [A.Type]) }
            : conid types                   { (mkInfo $1, id2conName $1, $2) }

tyvars      :: { [N.Name] }
            : varid tyvars                  { id2tyVarName $1 : $2 }
            | {- empty -}                   { [] }

expr        :: { A.Expr }
            : '\\' varid vars '->' expr                 { A.LamExpr (mkInfo $1) (id2varName $2 : $3) $5 }
            | 'let' '{' decls '}' 'in' expr             { A.LetExpr (mkInfo $1) $3 $6 }
            | 'case' expr 'of' '{' alts '}'             { A.CaseExpr (mkInfo $1) $2 $5 }
            | expr '[' types ']'                        { A.TAppExpr (mkInfo $2) $1 $3 }
            | expr aexpr                                { A.AppExpr $1 $2 }
            | aexpr                                     { $1 }

aexpr       :: { A.Expr }
            : '(' expr ')'                              { $2 }
            | varid                                     { A.VarExpr (mkInfo $1) (id2varName $1) }
            | conid                                     { A.VarExpr (mkInfo $1) (id2conName $1) }

vars        :: { [N.Name] }
            : varid vars                    { id2varName $1 : $2 }
            | {- empty -}                   { [] }

alts        :: { [(A.Pat, A.Expr)] }
            : alt ';' alts                  { $1 : $3 }
            | {- empty -}                   { [] }

alt         :: { (A.Pat, A.Expr) }
            : pat '->' expr                 { ($1, $3) }

pat         :: { A.Pat }
            : conid apats                   { A.ConPat (mkInfo $1) (id2conName $1) $2 }
            | apat                          { $1 }

apats        :: { [A.Pat] }
            : apat apats                    { $1 : $2 }
            | apat                          { [$1] }

apat        :: { A.Pat }
            : {-'(' pat ')'                 { $2 }
            |-} conid                       { A.ConPat (mkInfo $1) (id2conName $1) [] }
            | varid                         { A.VarPat (mkInfo $1) (id2varName $1) }
            | '_'                           { A.WildPat (mkInfo $1) }

{
parseError :: Token -> Alex a
parseError t = alexError $ "parse error: " ++ pretty t

id2varName :: (String, AlexPosn) -> Name
id2varName = N.str2varName . fst

id2conName :: (String, AlexPosn) -> Name
id2conName = N.str2conName . fst

id2tyVarName :: (String, AlexPosn) -> Name
id2tyVarName = N.str2tyVarName . fst

id2tyConName :: (String, AlexPosn) -> Name
id2tyConName = N.str2tyConName . fst

class MkInfo a where
    mkInfo :: a -> Info

instance MkInfo AlexPosn where
    mkInfo (AlexPn _ l c) = Info{ line = l, col = c }

instance MkInfo (String, AlexPosn) where
    mkInfo (_, p) = mkInfo p

instance MkInfo (Float, AlexPosn) where
    mkInfo (_, p) = mkInfo p

instance Pretty Token where
    pretty (TokKeyword (k, p)) = "'" ++ pretty k ++ "' at " ++ prettyPos p
    pretty (TokSymbol (s, p)) = "'" ++ pretty s ++ "' at " ++ prettyPos p
    pretty (TokVarId (s, p)) = "'" ++ s ++ "' at " ++ prettyPos p
    pretty (TokConId (s, p)) = "'" ++ s ++ "' at " ++ prettyPos p
    pretty (TokVarSym (s, p)) = "'" ++ s ++ "' at " ++ prettyPos p
    pretty (TokFloat (i, p)) = show i ++ " at " ++ prettyPos p
    pretty (TokString (s, p)) = "\"" ++ s ++ "\" at " ++ prettyPos p
    pretty TokEof = "<eof>"

instance Pretty AlexPosn where
    pretty (AlexPn _ l c) = show l ++ ":" ++ show c
}

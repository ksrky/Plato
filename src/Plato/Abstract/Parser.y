{
module Plato.Abstract.Parser where

import Plato.Abstract.Fixity as F
import Plato.Abstract.Lexer
import Plato.Abstract.Syntax as A
import Plato.Abstract.Token
import Plato.Common.Info
import Plato.Common.Name as N
import Plato.Common.Pretty
}

%name parse program
%tokentype { Token }
%monad { Alex }
%lexer { lexer } { TokEof }
%error { parseError }

%token

'case'                          { TokKeyword (KwCase, $$) }
'data'                          { TokKeyword (KwData, $$) }
'forall'                        { TokKeyword (KwForall, $$) }
'import'                        { TokKeyword (KwImport, _) }
'infix'                         { TokKeyword (KwInfix, _) }
'infixl'                        { TokKeyword (KwInfixL, _) }
'infixr'                        { TokKeyword (KwInfixR, _) }
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
'('                             { TokSymbol (SymLParen, $$) }
'}'                             { TokSymbol (SymRBrace, _) }
']'                             { TokSymbol (SymRBrack, _) }
')'                             { TokSymbol (SymRParen, _) }
';'                             { TokSymbol (SymSemicolon, _) }
'_'                             { TokSymbol (SymUScore, $$) }
'|'                             { TokSymbol (SymVBar, _) }

varid                           { TokVarId $$ }
conid                           { TokConId $$ }
qconid                          { TokQConId $$ }
varsym                          { TokVarSym $$ }
consym                          { TokConSym $$ }

int                             { TokInt ($$, _) }
string                          { TokString $$ }

%%

program     : 'module' modid ';' body               { A.Program (Just $2) (fst $4) (snd $4) }
            | body                                  { A.Program Nothing (fst $1) (snd $1) }

body        : impdecls ';' topdecls                 { ($1, $3) }
            | topdecls                              { ([], $1) }

impdecls    :: { [A.ImpDecl] }
            : impdecl ';' impdecls                  { $1 : $3 }
            | {- empty -}                           { [] }

impdecl     :: { A.ImpDecl }
            : 'import' modid                        { A.ImpDecl $2 }

modid       :: { N.ModuleName }
            : qconid                                { N.ModuleName (splitModid (fst $1)) }
            | conid                                 { N.ModuleName [(id2conName $1)] }

modid_      :: { [N.Name] }
            : conid '.' modid_                      { id2conName $1 : $3 }
            | conid                                 { [id2conName $1] }

topdecls    :: { [A.TopDecl] }
            : topdecl ';' topdecls                  { $1 : $3 }
            | {- empty -}                           { [] }

topdecl     :: { A.TopDecl }
            : 'data' conid tyvars '=' constrs       { A.DataDecl (mkInfo $1) (id2tyConName $2) $3 $5 }
            | 'data' conid tyvars                   { A.DataDecl (mkInfo $1) (id2tyConName $2) $3 [] }
            | conid tyvars '=' type                 { A.TypeDecl (mkInfo $1) (id2tyConName $1) $2 $4 }
            | fixdecl                               { A.FixDecl }
            | decl                                  { A.Decl $1 }

decls       :: { [A.Decl] }
            : decl ';'                              { [$1] }
            | decl ';' decls                        { $1 : $3 }

decl        :: { A.Decl }
            : varid ':' type                        { A.FuncTyDecl (mkInfo $1) (id2varName $1) $3 }
            | '(' varsym ')' ':' type               { A.FuncTyDecl (mkInfo $1) (id2varName $2) $5 }
            | varid vars '=' expr                   { A.FuncDecl (mkInfo $1) (id2varName $1) $2 $4 }
            | '(' varsym ')' vars '=' expr          { A.FuncDecl (mkInfo $1) (id2varName $2) $4 $6 }

fixdecl     :: { Alex () }
            : 'infix' int op                        { setFixity (snd $3) $2 F.Nonfix }
            | 'infixl' int op                       { setFixity (snd $3) $2 F.Leftfix }
            | 'infixr' int op                       { setFixity (snd $3) $2 F.Rightfix }

types       :: { [A.Type] }
            : atype types                           { $1 : $2 }
            | {- empty -}                           { [] }

type        :: { A.Type }
            : 'forall' tyvars '.' type              { A.AllType (mkInfo $1) $2 $4 }
            | btype '->' type                       { A.ArrType (mkInfo $2) $1 $3 }
            | '(' type ')'                          { $2 }
            | btype                                 { $1 }

btype       :: { A.Type }
            : btype atype                           { A.AppType $1 $2 }
            | atype                                 { $1 }

atype       :: { A.Type }
            : '(' type ')'                          { $2 }
            | conid                                 { A.ConType (mkInfo $1) (id2tyConName $1) }
            | varid                                 { A.VarType (mkInfo $1) (id2tyVarName $1) }

constrs     :: { [(Info, N.Name, [A.Type])] }
            : constr '|' constrs                    { $1 : $3 }
            | constr                                { [$1] }

constr      :: { (Info, N.Name, [A.Type]) }
            : conid types                           { (mkInfo $1, id2conName $1, $2) }

tyvars      :: { [N.Name] }
            : varid tyvars                          { id2tyVarName $1 : $2 }
            | {- empty -}                           { [] }

op          :: { (Info, N.Name) }
            : varsym                                { (mkInfo $1, id2varName $1) }
            | consym                                { (mkInfo $1, id2conName $1) }

expr        :: { A.Expr }
            : infixexpr                             { $1 }

infixexpr   :: { A.Expr }
            : infixexpr_                            {% mkInfix $1 }

infixexpr_  :: { [F.Tok] }
            : lexpr op infixexpr_                   {% do
                                                        oper <- getFixity $ (snd $2)
                                                        return $ F.TExp (F.Exp $1) : F.TOp (fst $2) oper : $3 }
            | lexpr                                 { [F.TExp (F.Exp $1)] }

lexpr       :: { A.Expr }
            : '\\' varid vars '->' expr             { A.LamExpr (mkInfo $1) (id2varName $2 : $3) $5 }
            | 'let' '{' decls '}' 'in' expr         { A.LetExpr (mkInfo $1) $3 $6 }
            | 'case' expr 'of' '{' alts '}'         { A.CaseExpr (mkInfo $1) $2 $5 }
            | fexpr                                 { $1 }

fexpr       :: { A.Expr }
            : fexpr aexpr                           { A.AppExpr $1 $2 }
            | fexpr '[' types ']'                   { A.TAppExpr (mkInfo $2) $1 $3 }
            | aexpr                                 { $1 }

aexpr       :: { A.Expr }
            : '(' expr ')'                          { $2 }
            | varid                                 { A.VarExpr (mkInfo $1) (id2varName $1) }
            | conid                                 { A.VarExpr (mkInfo $1) (id2conName $1) }
            | '(' expr op ')'                       { A.AppExpr (A.VarExpr (fst $3) (snd $3)) $2 }

vars        :: { [N.Name] }
            : varid vars                            { id2varName $1 : $2 }
            | {- empty -}                           { [] }

alts        :: { [(A.Pat, A.Expr)] }
            : alt ';' alts                          { $1 : $3 }
            | {- empty -}                           { [] }

alt         :: { (A.Pat, A.Expr) }
            : pat '->' expr                         { ($1, $3) }

pat         :: { A.Pat }
            : conid apats                           { A.ConPat (mkInfo $1) (id2conName $1) $2 }
            | apat                                  { $1 }

apats        :: { [A.Pat] }
            : apat apats                            { $1 : $2 }
            | apat                                  { [$1] }

apat        :: { A.Pat }
            : {-'(' pat ')'                         { $2 }
            |-} conid                               { A.ConPat (mkInfo $1) (id2conName $1) [] }
            | varid                                 { A.VarPat (mkInfo $1) (id2varName $1) }
            | '_'                                   { A.WildPat (mkInfo $1) }

{
parseError :: Token -> Alex a
parseError t = alexError $ "parse error: " ++ pretty t

id2varName :: (String, Posn) -> N.Name
id2varName = N.str2varName . fst

id2conName :: (String, Posn) -> N.Name
id2conName = N.str2conName . fst

id2tyVarName :: (String, Posn) -> N.Name
id2tyVarName = N.str2tyVarName . fst

id2tyConName :: (String, Posn) -> N.Name
id2tyConName = N.str2tyConName . fst

splitModid :: String -> [N.Name]
splitModid = loop 0 . (++ ".")
  where
    loop cnt [] = []
    loop cnt xs =
        if (xs !! cnt) == '.'
            then N.str2conName (take cnt xs) : loop 0 (drop (cnt + 1) xs)
            else loop (cnt + 1) xs

mkInfix :: [F.Tok] -> Alex A.Expr
mkInfix toks = case resolve toks of
    Just exp -> return $ F.exp2expr exp
    Nothing -> alexError "invalid infix operator" -- todo: parser monad should be outside of Lexer module

class MkInfo a where
    mkInfo :: a -> Info

instance MkInfo Posn where
    mkInfo (Pn l c) = Info{ line = l, col = c }

instance MkInfo (String, Posn) where
    mkInfo (_, p) = mkInfo p

instance MkInfo (Float, Posn) where
    mkInfo (_, p) = mkInfo p
}

{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Plato.Parsing.Parser where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc

import Plato.Parsing.Fixity
import Plato.Parsing.Layout
import Plato.Parsing.Lexer
import Plato.Parsing.Monad
import Plato.Parsing.Token

import Plato.Syntax.Parsing

import qualified Data.Text as T
import Control.Monad (unless)
import Control.Monad.State
import Control.Exception.Safe (MonadThrow)
import qualified Data.Map.Strict as M
import Prettyprinter
}

%tokentype { Located Token }
%monad { Parser } { (>>=) } { return }
%lexer { lexer } { L _ TokEOF }
%error { parseError }

%name parser program
%name exprParser expr
%name typeParser type
%name declsParser decls
%name topdeclParser topdecl

%token

'case'                          { L $$ (TokKeyword KwCase) }
'data'                          { L $$ (TokKeyword KwData) }
'forall'                        { L $$ (TokKeyword KwForall) }
'import'                        { L $$ (TokKeyword KwImport) }
'infix'                         { L $$ (TokKeyword KwInfix) }
'infixl'                        { L $$ (TokKeyword KwInfixL) }
'infixr'                        { L $$ (TokKeyword KwInfixR) }
'in'                            { L $$ (TokKeyword KwIn) }
'of'                            { L $$ (TokKeyword KwOf) }
'module'                        { L $$ (TokKeyword KwModule) }
'let'                           { L $$ (TokKeyword KwLet) }
'where'                         { L $$ (TokKeyword KwWhere) }

'->'                            { L $$ (TokSymbol SymArrow) }
'\\'                            { L $$ (TokSymbol SymBackslash) }
','                             { L $$ (TokSymbol SymComma) }
':'                             { L $$ (TokSymbol SymColon) }
'\''                            { L $$ (TokSymbol SymDash) }
'='                             { L $$ (TokSymbol SymEqual) }
'{'                             { L $$ (TokSymbol SymLBrace) }
'['                             { L $$ (TokSymbol SymLBrack) }
'('                             { L $$ (TokSymbol SymLParen) }
'}'                             { L $$ (TokSymbol SymRBrace) }
']'                             { L $$ (TokSymbol SymRBrack) }
')'                             { L $$ (TokSymbol SymRParen) }
';'                             { L $$ (TokSymbol SymSemicolon) }
'_'                             { L $$ (TokSymbol SymUScore) }
'|'                             { L $$ (TokSymbol SymVBar) }
'v{'                            { L $$ (TokSymbol SymVLBrace) }
'v}'                            { L $$ (TokSymbol SymVRBrace) }

varid                           { (mkLVarId -> Just $$) }
conid                           { (mkLConId -> Just $$) }
varsym                          { (mkLVarSym -> Just $$) }
consym                          { (mkLConSym -> Just $$) }

qconid                          { (mkLQConId -> Just $$) }

int                             { (mkLInt -> Just $$) }

%%

program     : ';' program_                          { $2 }

program_    : 'module' modid ';' body               { Program (Just $2) (fst $4) (snd $4) }
            | body                                  { Program Nothing (fst $1) (snd $1) }

body        : impdecls ';' topdecls                 { ($1, reverse $3) }
            | topdecls                              { ([], reverse $1) }

impdecls    :: { [Located ModuleName] }
            : impdecls ';' impdecl                  { $3 : $1 }
			| impdecl								{ [$1] }

impdecl     :: { Located ModuleName }
            : 'import' modid                        { $2 }

modid       :: { Located ModuleName }
            : qconid                                { cL $1 (ModuleName (splitModid $1)) }
            | conid                                 { cL $1 (ModuleName (splitModid $1)) }

-- | Declarations
topdecls    :: { [Located TopDecl] }
            : topdecls ';' topdecl                  { $3 : $1 }
            | topdecl                               { [$1] }

topdecl     :: { Located TopDecl }
            : 'data' conid tyvars '=' constrs       { cSLn $1 (snd $ last $5) (DataD (mkLtyconName $2) $3 $5) }
            | 'data' conid tyvars                   { cSLn $1 $3 (DataD (mkLtyconName $2) $3 []) }
            | conid tyvars '=' type                 { cLL $1 $4 (TypeD (mkLtyconName $1) $2 $4) }
            | fixdecl                               { $1 }
            | decl                                  { cL $1 (Decl $1) }
            | expr                                  { cL $1 (Eval $1)  }

fixdecl     :: { Located TopDecl }
            : 'infix' int op                        {% setFixity $3 $2 Nonfix >> return (L $1 FixD) }
            | 'infixl' int op                       {% setFixity $3 $2 Leftfix >> return (L $1 FixD) }
            | 'infixr' int op                       {% setFixity $3 $2 Rightfix >> return (L $1 FixD) }

decls       :: { [Located Decl] }
            : decl ';' decls                        { $1 : $3 }
            | decl                                  { [$1] }
            | {- empty -}                           { [] }

decl        :: { Located Decl }
            : var ':' type                        	{ cLL $1 $3 (FuncTyD $1 $3) }
            | '(' varsym ')' ':' type               { cSL $1 $5 (FuncTyD (mkLvarName $2) $5) }
            | var vars '=' expr                   	{ cLL $1 $4 (FuncD $1 $2 $4) }
            | '(' varsym ')' vars '=' expr          { cSL $1 $6 (FuncD (mkLvarName $2) $4 $6) }
			| var varsym var '=' expr				{ cLL $1 $5 (FuncD (mkLvarName $2) ([$1, $3]) $5) }

-- | Types
types       :: { [Located Type] }
            : atype types                           { $1 : $2 }
            | {- empty -}                           { [] }

type        :: { Located Type }
            : '{' tyvar tyvars '}' type             { cSL $1 $5 (AllT ($2 : $3) $5) }
            | btype '->' type                       { cLL $1 $3 (ArrT $1 $3) }
            | btype                                 { $1 }

btype       :: { Located Type }
            : btype atype                           { cLL $1 $2 (AppT $1 $2) }
            | atype                                 { $1 }

atype       :: { Located Type }
            : '(' type ')'                          { $2 }
            | conid                                 { cL $1 (ConT (mkLtyconName $1)) }
            | varid                                 { cL $1 (VarT (mkLtyvarName $1)) }

constrs     :: { [(Located Name, [Located Type])] }
            : constr '|' constrs                    { $1 : $3 }
            | constr                                { [$1] }

constr      :: { (Located Name, [Located Type]) }
            : conid types                           { (mkLconName $1, $2) }
            | '(' consym ')' types                  { (mkLconName $2, $4) }
			| type consym type						{ (mkLconName $2, [$1, $3])}

tyvars      :: { [Located Name] }
            : tyvar tyvars                          { $1 : $2 }
            | {- empty -}                           { [] }

tyvar		:: { Located Name }
			: varid									{ mkLtyvarName $1 }

op          :: { Located Name }
            : varsym                                { mkLvarName $1 }
            | consym                                { mkLconName $1 }

-- | Expressions
expr        :: { Located Expr }
            : lexpr op expr                         { cLL $1 $3 (OpE $1 $2 $3) }
            | lexpr                                 { $1 }

lexpr       :: { Located Expr }
            : '\\' var vars '->' expr               { cSL $1 $5 (LamE ($2 : $3) $5) }
            | 'let' '{' decls '}' 'in' expr         { cSL $1 $6 (LetE $3 $6) }
            | 'let' 'v{' decls close 'in' expr      { cSL $1 $6 (LetE $3 $6) }
            | 'case' expr 'of' '{' alts '}'         { L (combineSpans $1 $6) (CaseE $2 $5) }
            | 'case' expr 'of' 'v{' alts close      { L (combineSpans $1 $6) (CaseE $2 $5) }
            | fexpr                                 { $1 }

fexpr       :: { Located Expr }
            : fexpr aexpr                           { cLL $1 $2 (AppE $1 $2) }
            | aexpr                                 { $1 }

aexpr       :: { Located Expr }
            : '(' expr ')'                          { L (combineSpans $1 $3) (FactorE $2) }
            | '(' lexpr op ')'                      { L (combineSpans $1 $4) (AppE (cL $3 (VarE $3)) $2) }
            | varid                                 { cL $1 (VarE (mkLvarName $1)) }
            | conid                                 { cL $1 (VarE (mkLconName $1)) }

vars        :: { [Located Name] }
            : var vars                              { $1 : $2 }
            | {- empty -}                           { [] }

var  		:: { Located Name }
			: varid									{ mkLvarName $1 }

-- | Alternatives
alts        :: { [(Located Pat, Located Expr)] }
            : alt ';' alts                          { $1 : $3 }
            | alt                                   { [$1] }
            | {- empty -}                           { [] }

alt         :: { (Located Pat, Located Expr) }
            : pat '->' expr                         { ($1, $3) }

-- | Patterns
pat         :: { Located Pat }
            : lpat consym pat                       { cLL $1 $3 (ConP (mkLconName $2) ([$1, $3])) }
            | lpat                                  { $1 }

lpat		:: { Located Pat }
			: conid apats                           { cLLn $1 $2 (ConP (mkLconName $1) $2) }
            | apat									{ $1 }	

apats       :: { [Located Pat] }
            : apat apats                            { $1 : $2 }
            | apat                                  { [$1] }

apat        :: { Located Pat }
            : '(' pat ')'                         	{ $2 }
            | conid                               	{ cL $1 (ConP (mkLconName $1) []) }
            | varid                                 { cL $1 (VarP (mkLvarName $1)) }
            | '_'                                   { L $1 WildP }

-- | for parser-error(t) rule
close       :: { Span }
            : 'v}'                                  { $1 }
            | error                                 {% popLayoutLevel $1 }

{
parseError :: MonadThrow m => Located Token -> ParserT m a
parseError (L sp tok) = lift $ throwLocErr sp $ sep ["parse error at", pretty tok]

setFixity :: MonadThrow m => Located Name -> Located Int -> Fixity -> ParserT m ()
setFixity lop@(L _ op) (L sp prec) fix = do
    optab <- getOpTable
    unless (minPrec <= prec && prec <= maxPrec) $ lift $ throwLocErr sp $ sep ["invalid precedence", pretty prec]
    setOpTable $ M.insert op (Op lop prec fix) optab

splitModid :: Located T.Text -> [T.Text]
splitModid = loop 0 . ((`T.snoc` '.') <$>)
  where
    loop :: Int -> Located T.Text -> [T.Text]
    loop _ (L _ t) | null (T.unpack t) = []
    loop cnt (L sp t) =
        let xs = T.unpack t
         in if (xs !! cnt) == '.'
            then T.take cnt t : loop 0 (L sp (T.drop (cnt + 1) t))
            else loop (cnt + 1) (L sp t)

----------------------------------------------------------------
-- mk Located
----------------------------------------------------------------
mkLVarId :: Located Token -> Maybe (Located T.Text)
mkLVarId (L sp (TokVarId t)) = Just (L sp t)
mkLVarId _ = Nothing

mkLConId :: Located Token -> Maybe (Located T.Text)
mkLConId (L sp (TokConId t)) = Just (L sp t)
mkLConId _ = Nothing

mkLQConId :: Located Token -> Maybe (Located T.Text)
mkLQConId (L sp (TokQConId t)) = Just (L sp t)
mkLQConId _ = Nothing

mkLVarSym :: Located Token -> Maybe (Located T.Text)
mkLVarSym (L sp (TokVarSym t)) = Just (L sp t)
mkLVarSym _ = Nothing

mkLConSym :: Located Token -> Maybe (Located T.Text)
mkLConSym (L sp (TokConSym t)) = Just (L sp t)
mkLConSym _ = Nothing

mkLInt :: Located Token -> Maybe (Located Int)
mkLInt (L sp (TokInt n)) = Just (L sp n)
mkLInt _ = Nothing

mkLvarName :: Located T.Text -> Located Name
mkLvarName (L sp t)= L sp (varName t)

mkLconName :: Located T.Text -> Located Name
mkLconName (L sp t)= L sp (conName t)

mkLtyvarName :: Located T.Text -> Located Name
mkLtyvarName (L sp t)= L sp (tyvarName t)

mkLtyconName :: Located T.Text -> Located Name
mkLtyconName (L sp t)= L sp (tyconName t)

mkLVarExpr :: Located Name -> Located Expr
mkLVarExpr x = cL x (VarE x)

mkLAppExpr :: Located Expr -> Located Expr -> Located Expr
mkLAppExpr x y = cLL x y (AppE x y)
}
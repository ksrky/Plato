{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Plato.Parsing.Parser where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.Name.Reader
import Plato.Common.SrcLoc

import Plato.Parsing.Layout
import Plato.Parsing.Lexer
import Plato.Parsing.Monad
import Plato.Parsing.Token

import Plato.Syntax.Parsing

import Plato.Types.Fixity

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

qvarid                          { (mkLQVarId -> Just $$) }
qconid                          { (mkLQConId -> Just $$) }
qvarsym                         { (mkLQVarSym -> Just $$) }
qconsym                         { (mkLQConSym -> Just $$) }

int                             { (mkLInt -> Just $$) }

%%

program     : ';' program_                          { $2 }

program_    : 'module' modid ';' body               { Program (Just $2) (fst $4) (snd $4) }
            | body                                  { Program Nothing (fst $1) (snd $1) }

body        : impdecls ';' topdecls                 { ($1, reverse $3) }
            | topdecls                              { ([], reverse $1) }

-- | Imports
impdecls    :: { [Located ModuleName] }
            : impdecls ';' impdecl                  { $3 : $1 }
			| impdecl								{ [$1] }

impdecl     :: { Located ModuleName }
            : 'import' modid                        { $2 }

modid       :: { Located ModuleName }
            : qconid                                { cL $1 (ModuleName (splitOnDot $ unLoc $1)) }
            | conid                                 { cL $1 (ModuleName (splitOnDot $ unLoc $1)) }

-- | Declarations
topdecls    :: { [ LTopDecl RdrName ] }
            : topdecls ';' topdecl                  { $3 : $1 }
            | topdecl                               { [$1] }

topdecl     :: { LTopDecl RdrName }
            : 'data' ltycon tyvars0 '=' constrs     { cSLn $1 (snd $ last $5) (DataD $2 $3 $5) }
            | 'data' ltycon tyvars0                 { cSLn $1 $3 (DataD $2 $3 []) }
            | ltycon tyvars0 '=' type               { cLL $1 $4 (TypeD $1 $2 $4) }
            | decl                                  { cL $1 (Decl $1) }
            | expr                                  { cL $1 (Eval $1)  }

tyvars0     :: { [LName] }
            : tyvar tyvars0                         { $1 : $2 }
            | {- empty -}                           { [] }

decls       :: { [LDecl RdrName] }
            : decl ';' decls                        { $1 : $3 }
            | decl                                  { [$1] }
            | {- empty -}                           { [] }

decl        :: { LDecl RdrName }
            : lvar ':' type                        	{ cLL $1 $3 (FuncTyD $1 $3) }
            | '(' lvarop ')' ':' type               { cSL $1 $5 (FuncTyD $2 $5) }
            | lvar lvars '=' expr                 	{ cLL $1 $4 (FuncD $1 $2 $4) }
            | '(' lvarop ')' lvars '=' expr         { cSL $1 $6 (FuncD $2 $4 $6) }
            | lvar lvarop lvar '=' expr				{ cLL $1 $5 (FuncD $2 ([$1, $3]) $5) }
            | fixdecl                               { $1 }

fixdecl     :: { LDecl RdrName }
            : 'infix' int op                        {% cLL $1 $3 <$> setFixity $3 $2 Nonfix }
            | 'infixl' int op                       {% cLL $1 $3 <$> setFixity $3 $2 Leftfix }
            | 'infixr' int op                       {% cLL $1 $3 <$> setFixity $3 $2 Rightfix }

ops         :: { [LName] }
            : op ',' ops                            { $1 : $3 }
            | op                                    { [$1] }

op          :: { LName }
            : lvarop                                { $1 }
            | lconop                                { $1 }

-- | Types
types       :: { [LType RdrName] }
            : atype types                           { $1 : $2 }
            | {- empty -}                           { [] }

type        :: { LType RdrName }
            : '{' tyvar tyvars '}' type             { cSL $1 $5 (AllT ($2 : $3) $5) }
            | btype '->' type                       { cLL $1 $3 (ArrT $1 $3) }
            | btype                                 { $1 }

btype       :: { LType RdrName }
            : btype atype                           { cLL $1 $2 (AppT $1 $2) }
            | atype                                 { $1 }

atype       :: { LType RdrName }
            : '(' type ')'                          { $2 }
            | qtycon                                { cL $1 (ConT $1) }
            | tyvar                                 { cL $1 (VarT $1) }

constrs     :: { [(LName, [LType RdrName])] }
            : constr '|' constrs                    { $1 : $3 }
            | constr                                { [$1] }

constr      :: { (LName, [LType RdrName]) }
            : lcon types                            { ($1, $2) }
            | '(' lconop ')' types                  { ($2, $4) }
			| type lconop type						{ ($2, [$1, $3])}

-- | Expressions
expr        :: { LExpr RdrName }
            : lexpr qop expr                        { cLL $1 $3 (OpE $1 $2 $3) }
            | lexpr                                 { $1 }

lexpr       :: { LExpr RdrName }
            : '\\' lvars '->' expr                  { cSL $1 $5 (LamE $2 $4) }
            | 'let' '{' decls '}' 'in' expr         { cSL $1 $6 (LetE $3 $6) }
            | 'let' 'v{' decls close 'in' expr      { cSL $1 $6 (LetE $3 $6) }
            | 'case' expr 'of' '{' alts '}'         { L (combineSpans $1 $6) (CaseE $2 $5) }
            | 'case' expr 'of' 'v{' alts close      { L (combineSpans $1 $6) (CaseE $2 $5) }
            | fexpr                                 { $1 }

fexpr       :: { LExpr RdrName }
            : fexpr aexpr                           { cLL $1 $2 (AppE $1 $2) }
            | aexpr                                 { $1 }

aexpr       :: { LExpr RdrName }
            : '(' expr ')'                          { L (combineSpans $1 $3) (FactorE $2) }
            | '(' lexpr qop ')'                     { L (combineSpans $1 $4) (AppE (cL $3 (VarE $3)) $2) }
            | qvar                                  { cL $1 (VarE $1) }
            | qcon                                  { cL $1 (VarE $1) }

lvars       :: { [LName] }
            : lvar lvars                            { $1 : $2 }
            | lvar									{ $1 }

qop         :: { Located RdrName }
            : qvarop                                { $1 }
            | qconop                                { $1 }

-- | Alternatives
alts        :: { [(LPat, LExpr RdrName)] }
            : alt ';' alts                          { $1 : $3 }
            | alt                                   { [$1] }
            | {- empty -}                           { [] }

alt         :: { (LPat RdrName, LExpr RdrName) }
            : pat '->' expr                         { ($1, $3) }

-- | Patterns
pat         :: { LPat RdrName }
            : lpat conop pat                        { cLL $1 $3 (ConP $2 ([$1, $3])) }
            | lpat                                  { $1 }

lpat		:: { LPat RdrName }
			: con apats                             { cLLn $1 $2 (ConP $1 $2) }
            | apat									{ $1 }	

apats       :: { [LPat RdrName] }
            : apat apats                            { $1 : $2 }
            | apat                                  { [$1] }

apat        :: { LPat RdrName }
            : '(' pat ')'                         	{ $2 }
            | con                               	{ cL $1 (ConP $1 []) }
            | lvar                                  { cL $1 (VarP $1) }
            | '_'                                   { L $1 WildP }

-- | Names
qvar         :: { Located RdrName }
            : varid                                 { mkLRdrName varName $1 }
            | qvarid                                { mkLRdrName varName $1 }

lvar        :: { Located Name }
            : varid                                 { mkLName varName $1 }

qcon         :: { Located RdrName }
            : conid                                 { mkLRdrName conName $1 }
            | qconid                                { mkLRdrName conName $1 }   

lcon        :: { Located Name }
            : conid                                 { mkLName conName $1 }

tyvar       :: { Located Name }
            : varid                                 { mkLtyvarName $1 }

qtycon       :: { Located RdrName }
            : conid                                 { mkLRdrName tyconName $1 }
            | qconid                                { mkLRdrName tyconName $1 } 

ltycon      :: { Located Name }
            : conid                                 { mkLName tyconName $1 }

qvarop       :: { Located RdrName }
            : varsym                                { mkLRdrName varName $1 }
            | qvarsym                               { mkLRdrName varName $1 }

lvarop       :: { Located Name }
            : varsym                                { mkLName varName $1 }

qconop       :: { Located RdrName }
            : consym                                { mkLRdrName conName $1 }
            | qconsym                               { mkLRdrName conName $1 }   

lconop      :: { Located RdrName }
            : consym                                { mkLName conName $1 }  

-- | for parser-error(t) rule
close       :: { Span }
            : 'v}'                                  { $1 }
            | error                                 {% popLayoutLevel $1 }

{
parseError :: MonadThrow m => Located Token -> ParserT m a
parseError (L sp tok) = lift $ throwLocErr sp $ sep ["parse error at", pretty tok]

setFixity :: MonadThrow m => LName -> Located Int -> FixDir -> ParserT m Decl --tmp
setFixity lop@(L _ op) (L sp prec) fix = do
    fixenv <- getFixityEnv
    unless (minPrec <= prec && prec <= maxPrec) $ lift $ throwLocErr sp $ sep ["invalid precedence", pretty prec]
    setFixityEnv $ M.insert lop (Fixity prec fix) optab
    return $ FixityD fix prec op

splitOnDot :: T.Text -> [T.Text]
splitOnDot = (map T.pack) . (loop 0) . T.unpack
  where
    loop :: Int -> String -> [String]
    loop cnt xs
        | length xs == cnt = [xs]
        | otherwise = if (xs !! cnt) == '.'
            then take cnt xs : loop 0 (drop (cnt + 1) xs)
            else loop (cnt + 1) xs

----------------------------------------------------------------
-- mk Located
----------------------------------------------------------------
mkLVarId :: Located Token -> Maybe (Located T.Text)
mkLVarId (L sp (TokVarId t)) = Just (L sp t)
mkLVarId _ = Nothing

mkLConId :: Located Token -> Maybe (Located T.Text)
mkLConId (L sp (TokConId t)) = Just (L sp t)
mkLConId _ = Nothing

mkLVarSym :: Located Token -> Maybe (Located T.Text)
mkLVarSym (L sp (TokVarSym t)) = Just (L sp t)
mkLVarSym _ = Nothing

mkLConSym :: Located Token -> Maybe (Located T.Text)
mkLConSym (L sp (TokConSym t)) = Just (L sp t)
mkLConSym _ = Nothing

mkLQVarId :: Located Token -> Maybe (Located T.Text)
mkLQVarId (L sp (TokVarId t)) = Just (L sp t)
mkLQVarId _ = Nothing

mkLQConId :: Located Token -> Maybe (Located T.Text)
mkLQConId (L sp (TokQConId t)) = Just (L sp t)
mkLQConId _ = Nothing

mkLQVarSym :: Located Token -> Maybe (Located T.Text)
mkLQVarSym (L sp (TokQVarSym t)) = Just (L sp t)
mkLQVarSym _ = Nothing

mkLConQSym :: Located Token -> Maybe (Located T.Text)
mkLConQSym (L sp (TokQConSym t)) = Just (L sp t)
mkLConQSym _ = Nothing

mkLInt :: Located Token -> Maybe (Located Int)
mkLInt (L sp (TokInt n)) = Just (L sp n)
mkLInt _ = Nothing

mkLName :: (T.Text -> Name) -> Located Text -> Located Name
mkLName f (L sp t) = L sp (f x)

mkLRdrName :: (T.Text -> Name) -> Located Text -> Located RdrName
mkLRdrName f (L sp t) = case T.reverse (splitOnDot t) of
    [] -> unreachable ""
    [x] -> Unqual <$> f x
    x : xs -> Qual (ModuleName (T.reverse xs)) <$> f x
}
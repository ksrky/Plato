{
{-# LANGUAGE ViewPatterns #-}

module Plato.Parsing.Parser (
    parser,
    exprParser,
    typeParser,
    declsParser
) where

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name

import Plato.Parsing.Layout
import Plato.Parsing.Lexer
import Plato.Parsing.Monad
import Plato.Parsing.Token

import Plato.Syntax.Parsing

import Control.Monad (unless, forM)
import Control.Monad.State (lift)
import Control.Exception.Safe (MonadThrow)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
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

%token

'case'                          { L $$ (TokKeyword KwCase) }
'data'                          { L $$ (TokKeyword KwData) }
'import'                        { L $$ (TokKeyword KwImport) }
'infix'                         { L $$ (TokKeyword KwInfix) }
'infixl'                        { L $$ (TokKeyword KwInfixL) }
'infixr'                        { L $$ (TokKeyword KwInfixR) }
'in'                            { L $$ (TokKeyword KwIn) }
'of'                            { L $$ (TokKeyword KwOf) }
'open'                          { L $$ (TokKeyword KwOpen) }
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

qual                            { (mkLQual -> Just $$) }

int                             { (mkLInt -> Just $$) }

%%

program     :: { [LTopDecl] }
            : ';' topdecls                          { $2 }

-----------------------------------------------------------
-- Top declarations
-----------------------------------------------------------
topdecls    :: { [LTopDecl] }
            : decls                                 { $1 }

-----------------------------------------------------------
-- Evaluation expressions
-----------------------------------------------------------
evals       :: { [LTopDecl] }
            : {- not implemented -}                 { [] }

-----------------------------------------------------------
-- Declarations
-----------------------------------------------------------
decls       :: { [LDecl] }
            : decl ';' decls                        { $1 : $3 }
            | decl                                  { [$1] }
            | {- empty -}                           { [] }

decl        :: { LDecl }
            -- Data declaration
            : 'data' tycon tyvarrow 'where' '{' constrs '}'
                                                    { sL $1 $7 (DataD $2 $3 $6) }
            | 'data' tycon tyvarrow 'where' 'v{' constrs close
                                                    { sL $1 $7 (DataD $2 $3 $6) }
            | fundecl                               { L (getLoc $1) (FuncD (unLoc $1)) }

-- | Data declaration
constrs     :: { [(Ident, LType)] }
            : constr ';' constrs                    { $1 : $3 }
            | constr                                { [$1] }
            | {- empty -}                           { [] }

constr      :: { (Ident, LType) }
            : con ':' type                          { ($1, $3) }
            -- tmp: syntax restriction: last type of `type` must be its data type

-- | Function/signature declaration
fundecls    :: { [LFunDecl] }
            : fundecl ';' fundecls                  { $1 : $3 }
            | {- empty -}                           { [] }

fundecl     :: { LFunDecl }
            -- Function signature
            : var ':' type                        	{ sL $1 $3 (FunSpec $1 $3) }
            -- Function definition
            | var patrow '=' expr               	{ sL $1 $4 (FunBind $1 [($2, $4)]) }
            -- | var clauses

-----------------------------------------------------------
-- Types
-----------------------------------------------------------
type        :: { LType }
            : '{' tyvarrow '}' type                 { sL $1 $4 (AllT $2 $4) }
            | btype '->' type                       { sL $1 $3 (ArrT $1 $3) }
            | btype                                 { $1 }

btype       :: { LType }
            : {-btype atype                           { sL $1 $2 (AppT $1 $2) }
            |-} atype                                 { $1 }

atype       :: { LType }
            : '(' type ')'                          { $2 }
            | tycon                                 { L (getLoc $1) (ConT $1) }  --tmp: ? something wrong
            | tyvar                                 { L (getLoc $1) (VarT $1) }

-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------
expr        :: { LExpr }
            : lexpr                                 { $1 }

lexpr       :: { LExpr }
            -- | Lambda expression
            : '\\' patrow1 '->' expr                { sL $1 $4 (LamE $2 $4) }
            -- | Let expression
            | 'let' '{' fundecls '}' 'in' expr      { sL $1 $6 (LetE $3 $6) }
            | 'let' 'v{' fundecls close 'in' expr   { sL $1 $6 (LetE $3 $6) }
            -- | Function application
            | fexpr                                 { $1 }

fexpr       :: { LExpr }
            : fexpr aexpr                           { sL $1 $2 (AppE $1 $2) }
            | aexpr                                 { $1 }

aexpr       :: { LExpr }
            : var                                   { L (getLoc $1) (VarE $1) }
            | con                                   { L (getLoc $1) (VarE $1) }

-- | Alternatives
alts        :: { [(LPat, LExpr)] }
            : alt ';' alts                          { $1 : $3 }
            | alt                                   { [$1] }
            | {- empty -}                           { [] }

alt         :: { (LPat, LExpr) }
            : pat '->' expr                         { ($1, $3) }


-- | Clauses
clauses     :: { [Clause] }
            : 'where' '{' clauses_ '}'              { $3 }
            | 'where' 'v{' clauses_ 'v}'            { $3 }

clauses_    :: { [Clause] }
            : clause ';' clauses_                   { $1 : $3 }
            | clause                                { [$1] }
            | {- empty -}                           { [] }

clause      :: { Clause }
            : patrow1 '->' expr                     { ($1, $3) }

-----------------------------------------------------------
-- Patterns
-----------------------------------------------------------
pat         :: { LPat }
            : lpat                                  { $1 }
            -- tmp: infix pattern resolution 

lpat 		:: { LPat }
			: con apats                             { sL $1 $2 (ConP $1 $2) }
            | apat									{ $1 }	

apats       :: { [LPat] }
            : apat apats                            { $1 : $2 }
            | apat                                  { [$1] }

apat        :: { LPat }
            : '(' pat ')'                         	{ $2 }
            | con                               	{ L (getLoc $1) (ConP $1 []) }
            | var                                   { L (getLoc $1) (VarP $1) }
            | '_'                                   { L (getLoc $1) WildP }


-----------------------------------------------------------
-- Rows
-----------------------------------------------------------
patrow      :: { [LPat] }
            : apat patrow                           { $1 : $2 }
            | {- empty -}                           { [] }

patrow1     :: { [LPat] }
            : apat patrow                           { $1 : $2 }

tyvarrow    :: { [Ident] }
            : tyvar tyvarrow                        { $1 : $2 }
            | {- empty -}                           { [] }

typerow     :: { [LType] }
            : atype typerow                         { $1 : $2 }
            | {- empty -}                           { [] }

-----------------------------------------------------------
-- Identifiers
-----------------------------------------------------------
-- | Var
var         :: { Ident }
            : varid                                 {% mkIdent varName $1 }

-- | Con
con         :: { Ident }
            : conid                                 {% mkIdent conName $1 }

-- TyVar
tyvar       :: { Ident }
            : varid                                 {% mkIdent tyvarName $1 }

-- | TyCon
tycon       :: { Ident }
            : conid                                 {% mkIdent tyconName $1 }

-- | for parser-error(t) rule
close       :: { Span }
            : 'v}'                                  { $1 }
            | error                                 {% popLayoutLevel $1 }


{
parseError :: MonadThrow m => Located Token -> ParserT m a
parseError (L sp tok) = lift $ throwLocErr sp $ sep ["parse error at", pretty tok]

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

mkLQual :: Located Token -> Maybe (Located T.Text)
mkLQual (L sp (TokQual t)) = Just (L sp t)
mkLQual _ = Nothing

mkLInt :: Located Token -> Maybe (Located Int)
mkLInt (L sp (TokInt n)) = Just (L sp n)
mkLInt _ = Nothing

mkLName :: (T.Text -> Name) -> Located T.Text -> Located Name
mkLName f (L sp t) = L sp (f t)

mkIdent :: (T.Text -> Name) -> Located T.Text -> Parser Ident
mkIdent f x = do
    u <- freshUniq
    return $ ident (mkLName f x) u
}
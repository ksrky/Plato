{
{-# LANGUAGE ViewPatterns #-}

module Plato.Parsing.Parser (
    parser,
    exprParser,
    typeParser,
    declsParser,
    tokenParser,
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

import Control.Monad.State (lift)
import Control.Exception.Safe (MonadThrow)
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
%name tokenParser tokens

%token

'case'                          { L $$ (TokKeyword KwCase) }
'data'                          { L $$ (TokKeyword KwData) }
'import'                        { L $$ (TokKeyword KwImport) }
'in'                            { L $$ (TokKeyword KwIn) }
'infix'                         { L $$ (TokKeyword KwInfix) }
'infixl'                        { L $$ (TokKeyword KwInfixL) }
'infixr'                        { L $$ (TokKeyword KwInfixR) }
'let'                           { L $$ (TokKeyword KwLet) }
'of'                            { L $$ (TokKeyword KwOf) }
'where'                         { L $$ (TokKeyword KwWhere) }

'->'                            { L $$ (TokSymbol SymArrow) }
'\\'                            { L $$ (TokSymbol SymBackslash) }
':'                             { L $$ (TokSymbol SymColon) }
'\''                            { L $$ (TokSymbol SymDash) }
'='                             { L $$ (TokSymbol SymEqual) }
'{'                             { L $$ (TokSymbol SymLBrace) }
'('                             { L $$ (TokSymbol SymLParen) }
'}'                             { L $$ (TokSymbol SymRBrace) }
')'                             { L $$ (TokSymbol SymRParen) }
';'                             { L $$ (TokSymbol SymSemicolon) }
'_'                             { L $$ (TokSymbol SymUScore) }
'v{'                            { L $$ (TokSymbol SymVLBrace) }
'v}'                            { L $$ (TokSymbol SymVRBrace) }

varid                           { (mkLVarId -> Just $$) }
conid                           { (mkLConId -> Just $$) }
varsym                          { (mkLVarSym -> Just $$) }
consym                          { (mkLConSym -> Just $$) }

digit                           { (mkLDigit -> Just $$) }

%%

program     :: { Program }
            : ';' impdecls ';' topdecls             { reverse $2 ++ [$4] }
            | ';' topdecls                          { [$2] }

-----------------------------------------------------------
-- Imports
-----------------------------------------------------------
impdecls    :: { [LInstr] }
            : impdecls ';' impdecl                  { $3 : $1 } -- Note: throws error unless tail recursion
            | impdecl                               { [$1] }

impdecl     :: { LInstr }
            : 'import' conid                        { sL $1 $2 (ImpDecl $2) }

-----------------------------------------------------------
-- TopDecls
-----------------------------------------------------------
topdecls    :: { LInstr }
            : decls                                 { L (concatSpans (map getLoc $1)) (TopDecls $1) }

-----------------------------------------------------------
-- Declarations
-----------------------------------------------------------
decls       :: { [LTopDecl] }
            : decl ';' decls                        { $1 : $3 }
            | decl                                  { [$1] }
            | {- empty -}                           { [] }

decl        :: { LTopDecl }
            -- Data declaration
            : 'data' tycon tyvarseq datarhs
                                                    { sL $1 $4 (DataD $2 $3 (unLoc $4)) }
            | 'data' tyvar tyconop tyvar datarhs
                                                    { sL $1 $5 (DataD $3 [$2, $4] (unLoc $5)) }
            | 'data' '(' tyconop ')' tyvarseq datarhs
                                                    { sL $1 $6 (DataD $3 $5 (unLoc $6)) }
            | fundecl                               { $1 }
            | fixdecl                               { $1 }

-- | Data declaration
datarhs     :: { Located [(Ident, LType)] }
            : 'where' '{' constrs '}'               { sL $1 $4 $3 }
            | 'where' 'v{' constrs close            { sL $1 $4 $3 }

constrs     :: { [(Ident, LType)] }
            : constr ';' constrs                    { $1 : $3 }
            | constr                                { [$1] }
            | {- empty -}                           { [] }

constr      :: { (Ident, LType) }
            : con ':' type                          { ($1, $3) }
            | '(' conop ')' ':' type                { ($2, $5) }

-- | Function/signature declaration
fundecls    :: { [LDecl] }
            : fundecl ';' fundecls                  { $1 : $3 }
            | fundecl                               { [$1] }
            | {- empty -}                           { [] }

fundecl     :: { LDecl }
            -- Function signature
            : var ':' type                        	{ sL $1 $3 (FunSpecD $1 $3) }
            | '(' varop ')' ':' type                { sL $1 $5 (FunSpecD $2 $5) }
            -- Function definition
            | var patseq '=' expr               	{ sL $1 $4 (FunBindD $1 [($2, $4)]) }
            | '(' varop ')' patseq '=' expr         { sL $1 $6 (FunBindD $2 [($4, $6)]) }
            | lpat varop lpat '=' expr              { sL $1 $5 (FunBindD $2 [([$1, $3], $5)]) }
            | fixdecl                               { $1 }

-- | Fixity declaration
fixdecl     :: { LDecl }
            : 'infix' digit op                      { sL $1 $3 (FixityD $3 (Fixity (unLoc $2) Nonfix)) }
            | 'infixl' digit op                     { sL $1 $3 (FixityD $3 (Fixity (unLoc $2) Leftfix)) }
            | 'infixr' digit op                     { sL $1 $3 (FixityD $3 (Fixity (unLoc $2) Rightfix)) }

-----------------------------------------------------------
-- Types
-----------------------------------------------------------
type        :: { LType }
            : '{' tyvarseq1 '}' type                { sL $1 $4 (AllT $2 $4) }
            | ctype '->' type                       { sL $1 $3 (ArrT $1 $3) }
            | ctype                                 { $1 }

ctype       :: { LType }
            : btype tyconop ctype                   { sL $1 $3 (InfixT $1 $2 $3) }
            | btype                                 { $1 }

btype       :: { LType }
            : btype atype                           { sL $1 $2 (AppT $1 $2) }
            | atype                                 { $1 }

atype       :: { LType }
            : '(' type ')'                          { L (combineSpans $1 $3) (FactorT $2) }
            | tycon                                 { L (getLoc $1) (ConT $1) }  -- tmp: ? something wrong
            | tyvar                                 { L (getLoc $1) (VarT $1) }

-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------
expr        :: { LExpr }
            : lexpr op expr                         { sL $1 $3 (InfixE $1 $2 $3) }
            | lexpr                                 { $1 }

lexpr       :: { LExpr }
            -- | Lambda expression
            : '\\' patseq1 '->' expr                { sL $1 $4 (LamE $2 $4) }
            -- | Let expression
            | 'let' '{' fundecls '}' 'in' expr      { sL $1 $6 (LetE $3 $6) }
            | 'let' 'v{' fundecls close 'in' expr   { sL $1 $6 (LetE $3 $6) }
            -- | Case expression
            | 'case' expr 'of' '{' alts '}'         { sL $1 $6 (CaseE $2 $5) }
            | 'case' expr 'of' 'v{' alts 'v}'       { sL $1 $6 (CaseE $2 $5) }
            -- | Function application
            | fexpr                                 { $1 }

fexpr       :: { LExpr }
            : fexpr aexpr                           { sL $1 $2 (AppE $1 $2) }
            | aexpr                                 { $1 }

aexpr       :: { LExpr }
            : '(' op ')'                            { L (combineSpans $1 $3) (VarE $2) }
            | '(' expr ')'                          { L (combineSpans $1 $3) (FactorE $2) }
            | var                                   { L (getLoc $1) (VarE $1) }
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
            : patseq1 '->' expr                     { ($1, $3) }

-----------------------------------------------------------
-- Patterns
-----------------------------------------------------------
pat         :: { LPat }
            : lpat conop pat                        { sL $1 $3 (InfixP $1 $2 $3) } 
            | lpat                                  { $1 }

lpat 		:: { LPat }
			: con apats                             { sL $1 $2 (ConP $1 $2) }
            | apat									{ $1 }	

apats       :: { [LPat] }
            : apat apats                            { $1 : $2 }
            | apat                                  { [$1] }

apat        :: { LPat }
            : '(' pat ')'                         	{ L (combineSpans $1 $3) (FactorP $2) }
            | con                               	{ L (getLoc $1) (ConP $1 []) }
            | var                                   { L (getLoc $1) (VarP $1) }
            | '_'                                   { L (getLoc $1) WildP }


-----------------------------------------------------------
-- Sequence
-----------------------------------------------------------
patseq      :: { [LPat] }
            : apat patseq                           { $1 : $2 }
            | {- empty -}                           { [] }

patseq1     :: { [LPat] }
            : apat patseq                           { $1 : $2 }

tyvarseq    :: { [Ident] }
            : tyvar tyvarseq                        { $1 : $2 }
            | {- empty -}                           { [] }

tyvarseq1   :: { [Ident] }
            : tyvar tyvarseq                        { $1 : $2 }
            | tyvar                                 { [$1] }

typeseq     :: { [LType] }
            : atype typeseq                         { $1 : $2 }
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

-- | TyVar
tyvar       :: { Ident }
            : varid                                 {% mkIdent tyvarName $1 }

-- | TyCon
tycon       :: { Ident }
            : conid                                 {% mkIdent tyconName $1 }

-- | VarOp
varop       :: { Ident }
            : varsym                                {% mkIdent varName $1 }

-- | ConOp
conop       :: { Ident }
            : consym                                {% mkIdent conName $1 }

-- | TyconOp
tyconop     :: { Ident }
            : consym                                {% mkIdent tyconName $1 }

-- | Op
op          :: { Ident }
            : varop                                 { $1 }
            | conop                                 { $1 }

-- | for parser-error(t) rule
close       :: { Span }
            : 'v}'                                  { $1 }
            | error                                 {% popLayoutLevel $1 }

-- | Dummy parser
tokens      :: { [Token] }
            : token tokens                          { $1 : $2 }
            | {- empty -}                           { [] }

token       :: { Token }
            : 'case'                                { TokKeyword KwCase }
            | 'data'                                { TokKeyword KwData }
            | 'import'                              { TokKeyword KwImport }
            | 'in'                                  { TokKeyword KwIn }
            | 'infix'                               { TokKeyword KwInfix }
            | 'infixl'                              { TokKeyword KwInfixL }
            | 'infixr'                              { TokKeyword KwInfixR }
            | 'of'                                  { TokKeyword KwOf }
            | 'let'                                 { TokKeyword KwLet }
            | 'where'                               { TokKeyword KwWhere }
            | '->'                                  { TokSymbol SymArrow }
            | '\\'                                  { TokSymbol SymBackslash }
            | ':'                                   { TokSymbol SymColon }
            | '\''                                  { TokSymbol SymDash }
            | '='                                   { TokSymbol SymEqual }
            | '{'                                   { TokSymbol SymLBrace }
            | '('                                   { TokSymbol SymLParen }
            | '}'                                   { TokSymbol SymRBrace }
            | ')'                                   { TokSymbol SymRParen }
            | ';'                                   { TokSymbol SymSemicolon }
            | '_'                                   { TokSymbol SymUScore }
            | 'v{'                                  { TokSymbol SymVLBrace }
            | 'v}'                                  { TokSymbol SymVRBrace }
            | varid                                 { TokVarId (unLoc $1) }
            | conid                                 { TokConId (unLoc $1) }
            | varsym                                { TokVarSym (unLoc $1) }
            | consym                                { TokConSym (unLoc $1) }
            | digit                                 { TokDigit (unLoc $1) }

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

mkLDigit :: Located Token -> Maybe (Located Int)
mkLDigit (L sp (TokDigit n)) = Just (L sp n)
mkLDigit _ = Nothing

mkLName :: (T.Text -> Name) -> Located T.Text -> Located Name
mkLName f (L sp t) = L sp (f t)

mkIdent :: (T.Text -> Name) -> Located T.Text -> Parser Ident
mkIdent f x = do
    u <- freshUniq
    return $ ident (mkLName f x) u
}
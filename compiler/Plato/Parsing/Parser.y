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

digit                           { (mkLDigit -> Just $$) }

%%

program     :: { [LTopDecl] }
            : ';' topdecls                          { $2 }

-----------------------------------------------------------
-- Top declarations
-----------------------------------------------------------
topdecls    :: { [LTopDecl] }
            : decls                                 { $1 }

-----------------------------------------------------------
-- Declarations
-----------------------------------------------------------
decls       :: { [LDecl] }
            : decl ';' decls                        { $1 : $3 }
            | decl                                  { [$1] }
            | {- empty -}                           { [] }

decl        :: { LDecl }
            -- Data declaration
            : 'data' tycon tyvarseq 'where' '{' constrs '}'
                                                    { sL $1 $7 (DataD $2 $3 $6) }
            | 'data' tycon tyvarseq 'where' 'v{' constrs close
                                                    { sL $1 $7 (DataD $2 $3 $6) }
            | fundecl                               { L (getLoc $1) (FuncD [$1]) }

-- | Data declaration
constrs     :: { [(Ident, LType)] }
            : constr ';' constrs                    { $1 : $3 }
            | constr                                { [$1] }
            | {- empty -}                           { [] }

constr      :: { (Ident, LType) }
            : con ':' type                          { ($1, $3) }
            | '(' conop ')' ':' type                { ($2, $5) }

-- | Function/signature declaration
fundecls    :: { [LFunDecl] }
            : fundecl ';' fundecls                  { $1 : $3 }
            | fundecl                               { [$1] }
            | {- empty -}                           { [] }

fundecl     :: { LFunDecl }
            -- Function signature
            : var ':' type                        	{ sL $1 $3 (FunSpec $1 $3) }
            | '(' varop ')' ':' type                { sL $1 $5 (FunSpec $2 $5) }
            -- Function definition
            | var patrow '=' expr               	{ sL $1 $4 (FunBind $1 [($2, $4)]) }
            | '(' varop ')' patrow '=' expr         { sL $1 $6 (FunBind $2 [($4, $6)]) } -- tmp: synRstrc #patrow >= 2
            | pat varop pat '=' expr                { sL $1 $5 (FunBind $2 [([$1, $3], $5)]) }             
            | fixdecl                               { $1 }

fixdecl     :: { LFunDecl }
            : 'infix' digit op                      { sL $1 $3 (FixDecl $3 (Fixity (unLoc $2) Nonfix)) }
            | 'infixl' digit op                     { sL $1 $3 (FixDecl $3 (Fixity (unLoc $2) Leftfix)) }
            | 'infixr' digit op                     { sL $1 $3 (FixDecl $3 (Fixity (unLoc $2) Rightfix)) }

-----------------------------------------------------------
-- Types
-----------------------------------------------------------
type        :: { LType }
            : '{' tyvarseq1 '}' type                { sL $1 $4 (AllT $2 $4) }
            | btype '->' type                       { sL $1 $3 (ArrT $1 $3) }
            | btype                                 { $1 }

btype       :: { LType }
            : btype atype                           { sL $1 $2 (AppT $1 $2) }
            | atype                                 { $1 }

atype       :: { LType }
            : '(' type ')'                          { $2 }
            | tycon                                 { L (getLoc $1) (ConT $1) }  -- tmp: ? something wrong
            | tyvar                                 { L (getLoc $1) (VarT $1) }

-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------
expr        :: { LExpr }
            : lexpr op expr                         { sL $1 $3 (OpE $1 $2 $3) }
            | lexpr                                 { $1 }

lexpr       :: { LExpr }
            -- | Lambda expression
            : '\\' patrow1 '->' expr                { sL $1 $4 (LamE $2 $4) }
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
            | '(' expr ')'                          { $2 }
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
            : patrow1 '->' expr                     { ($1, $3) }

-----------------------------------------------------------
-- Patterns
-----------------------------------------------------------
pat         :: { LPat }
            : lpat conop pat                        { sL $1 $3 (ConP $2 [$1, $3]) }
            -- TODO: infix pattern resolution 
            | lpat                                  { $1 }

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
-- Sequence
-----------------------------------------------------------
patrow      :: { [LPat] }
            : apat patrow                           { $1 : $2 }
            | {- empty -}                           { [] }

patrow1     :: { [LPat] }
            : apat patrow                           { $1 : $2 }

tyvarseq    :: { [Ident] }
            : tyvar tyvarseq                        { $1 : $2 }
            | {- empty -}                           { [] }

tyvarseq1   :: { [Ident] }
            : tyvar tyvarseq                        { $1 : $2 }
            | tyvar                                 { [$1] }

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
            | 'infix'                               { TokKeyword KwInfix }
            | 'infixl'                              { TokKeyword KwInfixL }
            | 'infixr'                              { TokKeyword KwInfixR }
            | 'in'                                  { TokKeyword KwIn }
            | 'of'                                  { TokKeyword KwOf }
            | 'open'                                { TokKeyword KwOpen }
            | 'module'                              { TokKeyword KwModule }
            | 'let'                                 { TokKeyword KwLet }
            | 'where'                               { TokKeyword KwWhere }
            | '->'                                  { TokSymbol SymArrow }
            | '\\'                                  { TokSymbol SymBackslash }
            | ','                                   { TokSymbol SymComma }
            | ':'                                   { TokSymbol SymColon }
            | '\''                                  { TokSymbol SymDash }
            | '='                                   { TokSymbol SymEqual }
            | '{'                                   { TokSymbol SymLBrace }
            | '['                                   { TokSymbol SymLBrack }
            | '('                                   { TokSymbol SymLParen }
            | '}'                                   { TokSymbol SymRBrace }
            | ']'                                   { TokSymbol SymRBrack }
            | ')'                                   { TokSymbol SymRParen }
            | ';'                                   { TokSymbol SymSemicolon }
            | '_'                                   { TokSymbol SymUScore }
            | '|'                                   { TokSymbol SymVBar }
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
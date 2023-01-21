{
{-# LANGUAGE ViewPatterns #-}

module Plato.Parsing.Parser where

import Plato.Common.Error
import Plato.Common.Fixity
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
%name topdeclParser topdecl

%token

'case'                          { L $$ (TokKeyword KwCase) }
'data'                          { L $$ (TokKeyword KwData) }
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

program     :: { ([Located ModuleName], [LTopDecl]) }
            : ';' impdecls ';' topdecls             { (reverse $2, reverse $4) }

-- | Imports
impdecls    :: { [Located ModuleName] }
            : impdecls ';' impdecl                  { $3 : $1 }
            | impdecl                               { [$1] }

impdecl     :: { Located ModuleName }
            : 'import' modid                        { $2 }

modid       :: { Located ModuleName }
            : qconid                                { cL $1 (ModuleName (unLoc $1)) }
            | conid                                 { cL $1 (ModuleName (unLoc $1)) }

-- | Declarations
topdecls    :: { [ LTopDecl ] }
            : topdecls ';' topdecl                  { $3 : $1 }
            | topdecl                               { [$1] }

topdecl     :: { LTopDecl }
            : 'data' ltycon tyvars0 '=' constrs     { cSLn $1 (snd $ last $5) (DataD $2 $3 ($5)) }
            | 'data' ltycon tyvars0                 { cSLn $1 $3 (DataD $2 $3 []) }
            | decl                                  { cL $1 (Decl $1) }
            | expr                                  { cL $1 (Eval $1) }

constrs     :: { [(LName, [LType])] }
            : constr '|' constrs                    { $1 : $3 }
            | constr                                { [$1] }

constr      :: { (LName, [LType]) }
            : lcon types                            { ($1, $2) }
            | '(' lconop ')' types                  { ($2, $4) }
            | type lconop type                      { ($2, [$1, $3])}

tyvars0     :: { [LName] }
            : tyvar tyvars0                         { $1 : $2 }
            | {- empty -}                           { [] }

decls       :: { [LDecl] }
            : decls ';' decl                        { $3 : $1 }
            | decl                                  { [$1] }
            | {- empty -}                           { [] }

decl        :: { LDecl }
            : lvar ':' type                        	{ cLL $1 $3 (FuncTyD $1 $3) }
            | '(' lvarop ')' ':' type               { cSL $1 $5 (FuncTyD $2 $5) }
            | lvar lvars0 '=' expr                 	{ cLL $1 $4 (FuncD $1 $2 $4) }
            | '(' lvarop ')' lvars0 '=' expr        { cSL $1 $6 (FuncD $2 $4 $6) }
            | lvar lvarop lvar '=' expr				{ cLL $1 $5 (FuncD $2 ([$1, $3]) $5) }
            | fixdecl                               { $1 }

lvars0      :: { [LName] }
            : lvar lvars0                            { $1 : $2 }
            | {- empty -}                            { [] }

fixdecl     :: { LDecl }
            : 'infix' int ops                       {% do { fixd <- setFixities $3 $2 Nonfix; return (cSLn $1 $3 fixd) } }
            | 'infixl' int ops                      {% do { fixd <- setFixities $3 $2 Leftfix; return (cSLn $1 $3 fixd) } }
            | 'infixr' int ops                      {% do { fixd <- setFixities $3 $2 Rightfix; return (cSLn $1 $3 fixd) } }

ops         :: { [LName] }
            : op ',' ops                            { $1 : $3 }
            | op                                    { [$1] }

op          :: { LName }
            : lvarop                                { $1 }
            | lconop                                { $1 }

-- | Types
types       :: { [LType] }
            : atype types                           { $1 : $2 }
            | {- empty -}                           { [] }

type        :: { LType }
            : '{' tyvars '}' type                   { cSL $1 $4 (AllT $2 $4) }
            | btype '->' type                       { cLL $1 $3 (ArrT $1 $3) }
            | btype                                 { $1 }

btype       :: { LType }
            : btype atype                           { cLL $1 $2 (AppT $1 $2) }
            | atype                                 { $1 }

atype       :: { LType }
            : '(' type ')'                          { $2 }
            {-| qtycon                                { cL $1 (ConT $1) -}   --tmp: ?
            | conid                                 { cL $1 (ConT (mkLUnqual tyconName $1)) }
            | qconid                                { cL $1 (ConT (mkLQual tyconName $1)) }
            | tyvar                                 { cL $1 (VarT $1) }

tyvars      :: { [LName] }
            : tyvar tyvars                          { $1 : $2 }
            | tyvar                                 { [$1] }

-- | Expressions
expr        :: { LExpr }
            : lexpr qop expr                        { cLL $1 $3 (OpE $1 $2 $3) }
            | lexpr                                 { $1 }

lexpr       :: { LExpr }
            : '\\' lvars '->' expr                  { cSL $1 $4 (LamE $2 $4) }
            | 'let' '{' decls '}' 'in' expr         { cSL $1 $6 (LetE (reverse $3) $6) }
            | 'let' 'v{' decls close 'in' expr      { cSL $1 $6 (LetE (reverse $3) $6) }
            | 'case' expr 'of' '{' alts '}'         { L (combineSpans $1 $6) (CaseE $2 $5) }
            | 'case' expr 'of' 'v{' alts close      { L (combineSpans $1 $6) (CaseE $2 $5) }
            | fexpr                                 { $1 }

fexpr       :: { LExpr }
            : fexpr aexpr                           { cLL $1 $2 (AppE $1 $2) }
            | aexpr                                 { $1 }

aexpr       :: { LExpr }
            : '(' expr ')'                          { L (combineSpans $1 $3) (FactorE $2) }
            | '(' lexpr qop ')'                     { L (combineSpans $1 $4) (AppE (cL $3 (VarE $3)) $2) }
            | '(' qop ')'                           { L (combineSpans $1 $3) (VarE $2) }
            | qvar                                  { cL $1 (VarE $1) }
            | qcon                                  { cL $1 (VarE $1) }

lvars       :: { [LName] }
            : lvar lvars                            { $1 : $2 }
            | lvar                                  { [$1] }

qop         :: { LPsName }
            : qvarop                                { $1 }
            | qconop                                { $1 }

-- | Alternatives
alts        :: { [(LPat, LExpr)] }
            : alt ';' alts                          { $1 : $3 }
            | alt                                   { [$1] }
            | {- empty -}                           { [] }

alt         :: { (LPat, LExpr) }
            : pat '->' expr                         { ($1, $3) }

-- | Patterns
pat         :: { LPat }
            : lpat qconop pat                       { cLL $1 $3 (ConP $2 ([$1, $3])) }
            | lpat                                  { $1 }

lpat		:: { LPat }
			: qcon apats                            { cLLn $1 $2 (ConP $1 $2) }
            | apat									{ $1 }	

apats       :: { [LPat] }
            : apat apats                            { $1 : $2 }
            | apat                                  { [$1] }

apat        :: { LPat }
            : '(' pat ')'                         	{ $2 }
            | qcon                               	{ cL $1 (ConP $1 []) }
            | lvar                                  { cL $1 (VarP $1) }
            | '_'                                   { L $1 WildP }

-- | Names
qvar        :: { LPsName }
            : varid                                 { mkLUnqual varName $1 }
            | qvarid                                { mkLQual varName $1 }

lvar        :: { LName }
            : varid                                 { mkLName varName $1 }

qcon        :: { LPsName }
            : conid                                 { mkLUnqual conName $1 }
            | qconid                                { mkLQual conName $1 }   

lcon        :: { LName }
            : conid                                 { mkLName conName $1 }

tyvar       :: { LName }
            : varid                                 { mkLName tyvarName $1 }

qtycon      :: { LPsName }
            : conid                                 { mkLUnqual tyconName $1 }
            | qconid                                { mkLQual tyconName $1 } 

ltycon      :: { LName }
            : conid                                 { mkLName tyconName $1 }

qvarop      :: { LPsName }
            : varsym                                { mkLUnqual varName $1 }
            | qvarsym                               { mkLQual varName $1 }

lvarop      :: { LName }
            : varsym                                { mkLName varName $1 }

qconop      :: { LPsName }
            : consym                                { mkLUnqual conName $1 }
            | qconsym                               { mkLQual conName $1 }   

lconop      :: { LName }
            : consym                                { mkLName conName $1 }  

-- | for parser-error(t) rule
close       :: { Span }
            : 'v}'                                  { $1 }
            | error                                 {% popLayoutLevel $1 }


{
parseError :: MonadThrow m => Located Token -> ParserT m a
parseError (L sp tok) = lift $ throwLocErr sp $ sep ["parse error at", pretty tok]

setFixities :: MonadThrow m => [LName] -> Located Int -> FixDir -> ParserT m Decl
setFixities ops (L sp prec) fix = do
    unless (minPrec <= prec && prec <= maxPrec) $ lift $ throwLocErr sp $ hsep ["invalid precedence", pretty prec]
    ops' <- forM ops $ \op -> do
        fixenv <- getFixityEnv
        setFixityEnv $ M.insert (unLoc op) (Fixity prec fix) fixenv
        return op
    return $ FixityD fix prec ops'

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
mkLQVarId (L sp (TokQVarId qt)) = Just (L sp qt)
mkLQVarId _ = Nothing

mkLQConId :: Located Token -> Maybe (Located T.Text)
mkLQConId (L sp (TokQConId qt)) = Just (L sp qt)
mkLQConId _ = Nothing

mkLQVarSym :: Located Token -> Maybe (Located T.Text)
mkLQVarSym (L sp (TokQVarSym qt)) = Just (L sp qt)
mkLQVarSym _ = Nothing

mkLQConSym :: Located Token -> Maybe (Located T.Text)
mkLQConSym (L sp (TokQConSym qt)) = Just (L sp qt)
mkLQConSym _ = Nothing

mkLInt :: Located Token -> Maybe (Located Int)
mkLInt (L sp (TokInt n)) = Just (L sp n)
mkLInt _ = Nothing

mkLName :: (T.Text -> Name) -> Located T.Text -> Located Name
mkLName f (L sp t) = L sp (f t)

mkLUnqual :: (T.Text -> Name) -> Located T.Text -> LPsName
mkLUnqual f (L sp t) = L sp $ Unqual (L sp (f t))

mkLQual :: (T.Text -> Name) -> Located T.Text -> LPsName
mkLQual f (L sp t) = L sp $ Qual (L sp1 (ModuleName q)) (L sp2 (f x))
    where
        (q, x) = case reverse (T.splitOn "." t) of
            x : xs -> (T.concat (reverse xs), x)
            _ -> unreachable "Plato.Parsing.Parser.mkLQual"
        len = T.length q
        (sp1, sp2) = case sp of
            NoSpan -> (NoSpan, NoSpan)
            Span loc1@(Loc fp l c) loc2 -> (Span loc1 (Loc fp l (c + len)), Span (Loc fp l (c + len + 1)) loc2)
}
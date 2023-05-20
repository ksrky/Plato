{
{-# LANGUAGE TupleSections #-}
module Plato.Core.Parser (parse) where

import Plato.Common.Name
import Plato.Core.Env
import Plato.Core.Lexer
import Plato.Core.Monad
import Plato.Core.Typing
import Plato.Syntax.Core
}

%name parser
%tokentype { Token }
%monad { IO } { >>= } { return }
%error { parseError }

%token

'case'                  { TokCase }
'fix'                   { TokFix }
'in'                    { TokIn }
'of'                    { TokOf }
'let'                   { TokLet }

'→'                     { TokArrow }
'@'                     { TokAt }
'='                     { TokEq }
'.'                     { TokDot }
','                     { TokComma }
':'                     { TokColon }
'∀'                     { TokForall }
'λ'                     { TokLambda }
'Λ'                     { TokLambdaU }
'{'                     { TokLBrace }
'('                     { TokLParen }
'μ'                     { TokMu }
'}'                     { TokRBrace }
')'                     { TokRParen }
';'                     { TokSemi }
'*'                     { TokStar }

LCID             	{ TokLCID $$ }
UCID             	{ TokUCID $$ }

%%

Commands :: { Core [Command] }
        : Bind ';' Commands                             { do { (x, b) <- $1
                                                             ; (Bind (mkIFN x) b :) <#> extendNameWith x $3 } }
        | Term ';' Commands                             { do { cmd <- Eval <#> $1
                                                             ; (cmd :) <#> $3 } }
        | Bind                                          { do { (x, b) <- $1
                                                             ; return [Bind (mkIFN x) b] } }
        | Term                                          { do { cmd <- Eval <#> $1
                                                             ; return [cmd] } }
        | {- empty -}                                   { return [] }

Bind    :: { Core (Name, Binding) }
        : Var ':' Type                                  { ($1,) <#> (TmVarBind <#> $3) }
        | Tycon ':' Kind                                { ($1,) <#> (TyVarBind <#> $3) }
        | Var '=' Term                                  { do { t <- $3
                                                             ; tyT <- typeof t
                                                             ; return ($1, TmAbbBind t tyT)} }
        | Tycon '=' Type                                { do { tyT <- $3
                                                             ; knK <- kindof tyT
                                                             ; return ($1, TyAbbBind tyT knK)} }

Term	:: { Core Term }
	: 'λ' Var ':' Type '.' Term                     { TmAbs (mkIFN $2) <#> $4 <*> extendNameWith $2 $6 }
	| 'Λ' Tyvar ':' Kind '.' Term                   { TmTAbs (mkIFN $2) <#> $4 <*> extendNameWith $2 $6 }
        | 'let' Var '=' Term 'in' Term                  { TmLet (mkIFN $2) <#> $4 <*> extendNameWith $2 $6 }
        | 'fix' Term1                                   { TmFix <#> $2 }
	| Term2                                         { $1 }

Term2	:: { Core Term }
	: Term2 Term1                                   { TmApp <#> $1 <*> $2 }
        | Term2 '@' Type1                               { TmTApp <#> $1 <*> $3 }
	| Term1                                         { $1 }

Term1	:: { Core Term }
	: Var                                           { do { idx <- getVarIndex $1
                                                             ; return $ TmVar idx (mkIFN $1) } }
        -- | Term1 '.' Var                                 { TmProj <#> $1 <*> pure $3 }
        | '{' Rcd '}'                                   { TmRecord <#> $2 }
	| '(' Term ')'                                  { $2 }

Rcd     :: { Core [(Name, Term)] }
        : Var '=' Term ',' Rcd                          { (:) <#> (($1,) <#> $3) <*> $5 }
        | Var '=' Term                                  { (:) <#> (($1,) <#> $3) <*> pure [] }
        | {- empty -}                                   { return [] }

Type    :: { Core Type }
	: '∀' Tyvar ':' Kind '.' Type                   { TyAll (mkIFN $2) <#> $4 <*> extendNameWith $2 $6 }
        | 'λ' Tyvar ':' Kind '.' Type                   { TyAbs (mkIFN $2) <#> $4 <*> extendNameWith $2 $6 }
        | 'μ' Tyvar ':' Kind '.' Type                   { TyRec (mkIFN $2) <#> $4 <*> extendNameWith $2 $6 }
        | Type2 '→' Type                                { TyFun <#> $1 <*> $3 }
        | Type2                                         { $1 }

Type2   :: { Core Type }
        : Type2 Type1                                   { TyApp <#> $1 <*> $2 }
        | Type1                                         { $1 }

Type1	:: { Core Type }
        : Tycon                                         { do { idx <- getVarIndex $1
                                                             ; return $ TyVar idx (mkIFN $1) } }
        | '{' TyRcd '}'                                 { TyRecord <#> $2 }
	| '(' Type ')'                                  { $2 }

TyRcd   :: { Core [(Name, Type)] }
        : Var ':' Type ',' TyRcd                        { (:) <#> (($1,) <#> $3) <*> $5 }
        | Var ':' Type                                  { (:) <#> (($1,) <#> $3) <*> pure [] }
        | {- empty -}                                   { return [] }

Kind    :: { Core Kind }
        : Kind1 '→' Kind                                { KnFun <#> $1 <*> $3 }
        | Kind1                                         { $1 }

Kind1   :: { Core Kind }
        : '*'                                           { return KnStar }
        | '(' Kind ')'                                  { $2 }

Var     :: { Name }
        : LCID                                          { varName $1 }

Con     :: { Name }
        : UCID                                          { conName $1 }

Tyvar   :: { Name }
        : LCID                                          { tyvarName $1 }

Tycon   :: { Name }
        : UCID                                          { tyconName $1 }

{
(<#>) :: Functor f => (a -> b) -> f a -> f b
(<#>) = (<$>)

infixl 4 <#>

mkIFN :: Name -> NameInfo
mkIFN = mkInfoFromName

parseError :: [Token] -> IO a
parseError [] = fail "parse error at EOF"
parseError (t : _) = fail $ "parse error at " ++ show t

parse :: CoreEnv -> String -> IO [Command]
parse env inp = do
        m <- parser $ alexScanTokens inp
        runCore m env
}
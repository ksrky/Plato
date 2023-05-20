{
module Plato.Core.Lexer where

import Data.Text qualified as T
}

%wrapper "basic"

$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [a-zA-Z]

@lcid = $lower [$alpha $digit \_ \']*
@ucid = $upper [$alpha $digit \_ \']*

tokens :-

$white+                 ;

case                    { \_ -> TokCase }
fix                     { \_ -> TokFix }
in                      { \_ -> TokIn }
of                      { \_ -> TokOf }
let                     { \_ -> TokLet }

\→                      { \_ -> TokArrow }
\@                      { \_ -> TokAt }
\=                      { \_ -> TokEq }
\.                      { \_ -> TokDot }
\,                      { \_ -> TokComma }
\:                      { \_ -> TokColon }
\∀                      { \_ -> TokForall }
\λ                      { \_ -> TokLambda }
\Λ                      { \_ -> TokLambdaU }
\{                      { \_ -> TokLBrace }
\(                      { \_ -> TokLParen }
\μ                      { \_ -> TokMu }
\}                      { \_ -> TokRBrace }
\)                      { \_ -> TokRParen }
\;                      { \_ -> TokSemi }
\*                      { \_ -> TokStar }

@lcid                   { \s -> TokLCID (T.pack s) }
@ucid                   { \s -> TokUCID (T.pack s) }

{
data Token
    = TokCase
    | TokFix
    | TokIn
    | TokOf
    | TokLet
    
    | TokArrow
    | TokAt
    | TokEq
    | TokDot
    | TokComma
    | TokColon
    | TokForall
    | TokLambda
    | TokLambdaU
    | TokLBrace
    | TokLParen
    | TokMu
    | TokRBrace
    | TokRParen
    | TokSemi
    | TokStar

    | TokLCID T.Text
    | TokUCID T.Text
    deriving (Eq, Show)
}
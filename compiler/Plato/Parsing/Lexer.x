{
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}

module Plato.Parsing.Lexer where

import Control.Monad (when)
import Control.Monad.State (lift)

import Plato.Parsing.Action
import Plato.Parsing.Error
import Plato.Parsing.Layout
import Plato.Parsing.Monad
import Plato.Parsing.Token

import Plato.Common.Error
import Plato.Common.Location
}


$nl = [\n\r\f]
$white_nonl = $white # \n

$small = [a-z]
$large = [A-Z]
$alpha = [a-zA-Z]
$digit = 0-9

$special = [\(\)\;\{\}\_\"\']
$common = [\!\#\$\%\&\*\+\.\,\/\<\=\>\?\@\\\^\`\|\-\~\[\]\:]
$symbol = $common

@varid = $small [$alpha $digit \_ \']*
@conid = $large [$alpha $digit \_ \']*

@varsym = ($symbol # \:) $symbol*
@consym = \: $symbol+

@decimal = $digit+

tokens :-
--  start code is alphabetic order:
--    code = 1
--    comment = 2
--    layout = 3

<0> $nl+                        ;
<0> $white_nonl+                { spaces }
<code> $white_nonl+             ;
<code> $nl+                     { begin 0 }
<layout> $nl+                   ;
<layout> $white_nonl*           { layoutSpaces }

-- | line comment
<0, code, layout>
    "--" \-* ~$symbol .*        ;

-- | block comment
<0, code, comment> "{-"         { beginComment }
<comment> "-}"                  { endComment }
<comment> $printable+           ;
<comment> $nl+                  ;


-- | keywords
<code> case                     { keyword KwCase }
<0, code> data                  { keyword KwData }
<0, code> import                { keyword KwImport }
<code> in                       { keyword KwIn }
<0, code> infix                 { keyword KwInfix }
<0, code> infixl                { keyword KwInfixL }
<0, code> infixr                { keyword KwInfixR }
<code> let                      { layoutKeyword KwLet }
<code> of                       { layoutKeyword KwOf }
<code> where                    { layoutKeyword KwWhere }

--| special symbols
<code> \'                       { symbol SymDash }
<0, code> \{                    { leftBrace }
<0, code> \(                    { symbol SymLParen }
<0, code> \}                    { rightBrace }
<0, code> \)                    { symbol SymRParen }
<0, code> \;                    { symbol SymSemicolon }
<0, code> \_                    { symbol SymUScore }

<code> @varsym                  { varsym }

--| common symbols
<code> \-\>                     { symbol SymArrow }
<code> \\                       { symbol SymBackslash }
<code> \:                       { symbol SymColon }
<code> \=                       { symbol SymEqual }

<0, code> @varid                { varid }
<0, code> @conid                { conid }
<code> @varsym                  { varsym }
<code> @consym                  { consym }

<code> $digit                   { digit }
{
lexer :: (Located Token -> Parser a) -> Parser a
lexer = (alexMonadScan >>=)

alexMonadScan :: Parser (Located Token)
alexMonadScan = do
    ainp@(pos, _, _, inp) <- getInput
    scd <- getStartCode
    lev <- getIndentLevels
    sp <- mkSpan pos inp 0
    case alexScan ainp scd of
        AlexEOF -> do
            cd <- getCommentDepth
            when (cd > 0) $ throwLexError sp "unterminated block comment"
            case lev of
                -- note: Layout rule
                -- L [] []                 = []
                -- L [] (m : ms)           = <closing brace>  :  L [] ms                     if m≠0
                -- alex bug: closing brace inside a comment throws parse error
                [] -> return $ L sp TokEOF
                0 : _ -> throwLexError sp "closing brace missing"
                _ : ms -> do
                    setIndentLevels ms
                    return $ L sp (TokSymbol SymVRBrace)
        AlexError _ -> throwLexError sp "lexical error"
        AlexSkip ainp' _len -> do
            setInput ainp'
            alexMonadScan
        AlexToken ainp' len action -> do
            setInput ainp'
            action ainp len
}
{
module Plato.Abstract.Lexer where

import Plato.Abstract.Fixity
import Plato.Abstract.Token
import Plato.Common.Name

import qualified Data.Map.Strict as M
import Control.Monad
}

%wrapper "monadUserState"

$tab = \t
$nl = [\n\r\f]
$white_nonl = $white # \n

$small = [a-z]
$large = [A-Z]
$alpha = [a-zA-Z]
$digit = 0-9
$symbol = [\.\@\<\=\>\&\|\!\$\*\+\?\#\~\-\[\]\^\/]

@reservedid = case | data | forall | import | in
            | of | let | module | where
@varid = $small [$alpha $digit \_ \']*
@conid = $large [$alpha $digit \_ \']*

@reservedop = \-\> | \= | \| | \:
@varsym = ($symbol # \:) $symbol*
@consym = \: $symbol*

@qual = (@conid \.)+
@qvarid = @qual @varid
@qconid = @qual @conid
@qvarsym = @qual @varsym
@qconsym = @qual @consym

@string = ($printable # \")*
@decimal = $digit+

tokens :-

<0> $white_nonl+               ;
<0> $nl                         ;

<0> "--".* $nl                  ;
<0, comment> "{-"               { beginComment }
<comment> [^$white]*"-}"        { endComment }
<comment> [^$white]+            ;

<0> case                        { keyword KwCase }
<0> data                        { keyword KwData }
<0> forall                      { keyword KwForall }
<0> import                      { keyword KwImport }
<0> infix                       { keyword KwInfix }
<0> infixl                      { keyword KwInfixL }
<0> infixr                      { keyword KwInfixR }
<0> in                          { keyword KwIn }
<0> of                          { keyword KwOf }
<0> let                         { keyword KwLet }
<0> module                      { keyword KwModule }
<0> where                       { keyword KwWhere }

<0> \'                          { symbol SymApost }
<0> \-\>                        { symbol SymArrow }
<0> \\                          { symbol SymBackslash }
<0> \:                          { symbol SymColon }
<0> \,                          { symbol SymComma }
<0> \.                          { symbol SymDot }
<0> \=                          { symbol SymEqual }
<0> \{                          { symbol SymLBrace }
<0> \[                          { symbol SymLBrack }
<0> \(                          { symbol SymLParen }
<0> \}                          { symbol SymRBrace }
<0> \]                          { symbol SymRBrack }
<0> \)                          { symbol SymRParen }
<0> \;                          { symbol SymSemicolon }
<0> \_                          { symbol SymUScore }         
<0> \|                          { symbol SymVBar }

<0> @varid                      { varid }
<0> @conid                      { conid }
<0> @qconid                     { qconid }
<0> @varsym                     { varsym }
<0> @consym                     { consym }
<0> @decimal                    { lex_int }

<0> \"                          { begin string }
<string> @string \"             { lex_string }

{
lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

type Action = AlexAction Token -- AlexInput -> Int -> Alex Token

toposn :: AlexPosn -> Posn
toposn (AlexPn _ l c) = Pn l c

keyword :: Keyword -> Action
keyword key = \(pos,_,_,_) _ -> return $ TokKeyword (key, toposn pos)

symbol :: Symbol -> Action
symbol sym = \(pos,_,_,_) _ -> return $ TokSymbol (sym, toposn pos)

varid :: Action
varid = \(pos,_,_,str) len -> return $ TokVarId (take len str, toposn pos)

conid :: Action
conid = \(pos,_,_,str) len -> return $ TokConId (take len str, toposn pos)

qconid :: Action
qconid = \(pos,_,_,str) len -> return $ TokQConId (take len str, toposn pos)

varsym :: Action
varsym = \(pos,_,_,str) len -> return $ TokVarSym (take len str, toposn pos)

consym :: Action
consym = \(pos,_,_,str) len -> return $ TokConSym (take len str, toposn pos)

lex_int :: Action
lex_int = \(pos,_,_,str) len -> return $ TokInt (read $ take len str, toposn pos)

lex_string :: Action
lex_string = \(pos,_,_,str) len -> do
    alexSetStartCode 0
    return $ TokString (init $ take len str, toposn pos)

beginComment :: Action
beginComment _ _ = do
    depth <- getCommentDepth 
    setCommentDepth (depth + 1)
    alexSetStartCode comment
    alexMonadScan

endComment :: Action
endComment _ _ = do
    depth <- getCommentDepth
    if depth == 0
        then alexError "lexical error: comment ended without starting."
        else do
            setCommentDepth (depth - 1)
            if depth == 1
                then alexSetStartCode 0
                else alexSetStartCode comment
            alexMonadScan

alexEOF :: Alex Token
alexEOF = do
    depth <- getCommentDepth
    if depth == 0
        then return TokEof
        else alexError "lexical error: unterminated block comment."

data AlexUserState = AlexUserState {
      commentDepth :: Int
    , fixityDict :: M.Map Name Op
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
      commentDepth  = 0
    , fixityDict = M.empty
}

getCommentDepth :: Alex Int
getCommentDepth = commentDepth <$> alexGetUserState

setCommentDepth :: Int -> Alex ()
setCommentDepth ss = do
    ust <- alexGetUserState
    alexSetUserState ust{ commentDepth = ss }

getFixity :: Name -> Alex Op
getFixity op = do
    dict <- fixityDict <$> alexGetUserState
    case M.lookup op dict of
        Just fixity -> return fixity
        Nothing -> return $ Op op 9 Leftfix -- alexError $ "operator " ++ show op ++ " is not defined"

setFixity :: Name -> Prec -> Fixity -> Alex ()
setFixity op prec fix = do
    ust <- alexGetUserState
    unless (0 <= prec && prec <= 9) $ alexError $ "invalid precedence " ++ show prec
    alexSetUserState ust{ fixityDict = M.insert op (Op op prec fix) (fixityDict ust) }
}

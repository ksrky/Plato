{
module Plato.Syntax.Lexer where
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
$tab = \t
$nl = [\n\r\f]
$white_no_nl = $white # \n

$small = [a-z]
$large = [A-Z]
$special    = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]

@reservedid = case | data | else | if | in
            | of | let | then | if | type | where
@varid = $small [$alpha $digit \_ \']*
@conid = $large [$alpha $digit \_ \']*

@reservedop = \-\> | \= | \| | \:
@varsym = $special+

@string = ($printable # \")*
@decimal = $digit+

tokens :-

<0> $white_no_nl+               ;
<0> $nl                         ;

<0> "--".* $nl                  ;
<0, comment> "{-"               { beginComment }
<comment> [^$white]*"-}"        { endComment }
<comment> [^$white]+            ;

<0> case                        { keyword KwCase }
<0> data                        { keyword KwData }
<0> in                          { keyword KwIn }
<0> of                          { keyword KwOf }
<0> let                         { keyword KwLet }
<0> type                        { keyword KwType }
<0> where                       { keyword KwWhere }

<0> \-\>                        { symbol SymArrow }
<0> \\                          { symbol SymBackslash }
<0> \:                          { symbol SymColon }
<0> \,                          { symbol SymComma }
<0> \=                          { symbol SymEqual }
<0> \(                          { symbol SymLParen }
<0> \)                          { symbol SymRParen }
<0> \;                          { symbol SymSemicolon }
<0> \|                          { symbol SymVBar }

<0> @varid                      { varid }
<0> @conid                      { conid }
<0> @varsym                     { varsym }
<0> @decimal                    { lex_int }

<0> \"                          { begin string }
<string> @string \"             { lex_string }

{
lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

type Action = AlexAction Token -- AlexInput -> Int -> Alex Token

data Token
    = TokKeyword (Keyword, AlexPosn)
    | TokSymbol (Symbol, AlexPosn)
    | TokVarId (String,  AlexPosn)
    | TokConId (String,  AlexPosn)
    | TokVarSym (String, AlexPosn)
    | TokInt (Integer, AlexPosn)
    | TokString (String, AlexPosn)
    | TokVOBrace
    | TokVCBrace
    | TokEof
    deriving (Eq, Show)

data Keyword
    = KwCase
    | KwData
    | KwIn
    | KwOf
    | KwLet
    | KwType
    | KwWhere
    deriving (Eq, Show)

data Symbol
    = SymArrow
    | SymBackslash
    | SymColon
    | SymComma
    | SymEqual
    | SymLParen
    | SymRParen
    | SymSemicolon -- tmp
    | SymVBar
    deriving (Eq, Show)

keyword :: Keyword -> Action
keyword key = \(pos,_,_,_) _ -> return $ TokKeyword (key, pos)

symbol :: Symbol -> Action
symbol sym = \(pos,_,_,_) _ -> return $ TokSymbol (sym, pos)

varid :: Action
varid = \(pos,_,_,str) len -> return $ TokVarId (take len str, pos)

conid :: Action
conid = \(pos,_,_,str) len -> return $ TokConId (take len str, pos)

varsym :: Action
varsym = \(pos,_,_,str) len -> return $ TokVarSym (take len str, pos)

lex_int :: Action
lex_int = \(pos,_,_,str) len -> return $ TokInt (read $ take len str, pos)

lex_string :: Action
lex_string = \(pos,_,_,str) len -> do
    alexSetStartCode 0
    return $ TokString (init $ take len str, pos)

beginComment :: Action
beginComment _ _ = do
    depth <- getLexerCommentDepth 
    setLexerCommentDepth (depth + 1)
    alexSetStartCode comment
    alexMonadScan

endComment :: Action
endComment _ _ = do
    depth <- getLexerCommentDepth
    if depth == 0
        then alexError "lexical error: comment ended without starting."
        else do
            setLexerCommentDepth (depth - 1)
            if depth == 1
                then alexSetStartCode 0
                else alexSetStartCode comment
            alexMonadScan

alexEOF :: Alex Token
alexEOF = do
    depth <- getLexerCommentDepth
    if depth == 0
        then return TokEof
        else alexError "lexical error: unterminated block comment."

data AlexUserState = AlexUserState {
      lexerCommentDepth  :: Int
    , lexerStringValue   :: String
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
      lexerCommentDepth  = 0
    , lexerStringValue   = ""
}

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = lexerCommentDepth <$> alexGetUserState

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = do
    ust <- alexGetUserState
    alexSetUserState ust{ lexerCommentDepth = ss }
}

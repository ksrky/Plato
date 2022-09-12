module Plato.Parsing.Layout where

import Plato.Common.Pretty
import Plato.Common.SrcLoc
import Plato.Parsing.Action
import Plato.Parsing.Error
import {-# SOURCE #-} Plato.Parsing.Lexer
import Plato.Parsing.Monad
import Plato.Parsing.Token

import Control.Monad.State
import qualified Data.Text as T

----------------------------------------------------------------
-- Layout
-- ref: https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3
----------------------------------------------------------------
layoutKeyword :: Keyword -> Action
layoutKeyword key (pos, _, _, inp) len = do
        let sp = mkSpan pos inp len
        setStartCode layout
        return $ L sp (TokKeyword key)

spaces :: Action -- strat code = 0
spaces ainp@(pos, _, _, inp) len = do
        il <- getIndentLevels
        let sp = mkSpan pos inp 0
        case il of
                m : ms
                        | m == len -> do
                                -- note: Layout rule
                                -- L (< n >: ts) (m : ms)  = ;  :  (L ts (m : ms))             if m = n
                                setStartCode code
                                setIndentLevels (m : ms)
                                return $ L sp (TokSymbol SymSemicolon)
                        | len < m -> do
                                -- note: Layout rule
                                -- L (< n >: ts) (m : ms)  = }  :  (L (< n >: ts) ms)          if n < m
                                setStartCode 0
                                setInput ainp
                                setIndentLevels ms
                                return $ L sp (TokSymbol SymVRBrace)
                _ -> do
                        ---------- note: Layout rule
                        ---------- L (< n >: ts) ms        = L ts ms                           if n > m
                        setStartCode code
                        alexMonadScan

layoutSpaces :: Action
layoutSpaces (pos, _, _, inp) len = do
        setStartCode code
        il <- getIndentLevels
        let sp = mkSpan pos inp 0
        case il of
                _ | T.unpack inp !! len == '{' -> do
                        -- note: Layout rule
                        -- If a let, where, do, or of keyword is not followed by the lexeme {,
                        -- the token {n} is inserted after the keyword, where n is the indentation
                        -- of the next lexeme if there is one, or 0 if the end of file has been reached.
                        setStartCode code
                        alexMonadScan
                m : ms | len > m -> do
                        -- note: Layout rule
                        -- L ({n} : ts) (m : ms)   = {  :  (L ts (n : m : ms))         if n > m
                        setIndentLevels (len : m : ms)
                        return $ L sp (TokSymbol SymVLBrace)
                [] | len > 0 -> do
                        -- note: Layout rule
                        -- L ({n} : ts) []         = {  :  (L ts [n])                  if n > 0
                        setIndentLevels [len]
                        return $ L sp (TokSymbol SymVLBrace)
                _ -> do
                        -- note: Layout rule
                        -- L ({n} : ts) ms         = {  :  }  :  (L (< n >: ts) ms)
                        return $ L sp (TokSymbol SymVLBrace) -- tmp:  L sp (TokSymbol SymVRBrace)]

rightBrace :: Action
rightBrace (pos, _, _, inp) len = do
        setStartCode code
        let sp = mkSpan pos inp len
        il <- getIndentLevels
        case il of
                0 : _ -> do
                        -- note: Layout rule
                        -- L (} : ts) (0 : ms)     = }  :  (L ts ms)
                        return $ L sp (TokSymbol SymRBrace)
                _ -> do
                        -- note: Layout rule
                        -- L (} : ts) ms           = parse-error
                        lift $ throwPsError sp "missing an opening brace before closing"

leftBrace :: Action
leftBrace (pos, _, _, inp) len = do
        -- note: Layout rule
        -- L ({ : ts) ms           = {  :  (L ts (0 : ms))
        setStartCode code
        let sp = mkSpan pos inp len
        il <- getIndentLevels
        setIndentLevels (0 : il)
        return $ L sp (TokSymbol SymLBrace)

-- Layout rule
--      L (t : ts) (m : ms)     = }  :  (L (t : ts) ms)             if m≠0 and parse-error(t)
popLayoutLevel :: Located Token -> Parser Span
popLayoutLevel (L sp _) = do
        il <- getIndentLevels
        scd <- getStartCode
        case il of
                m : ms | m /= 0 -> do
                        setIndentLevels ms
                        return sp
                _ -> lift $ throwPsError sp "parse error"

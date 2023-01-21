module Plato.Parsing.Layout where

import Plato.Parsing.Action
import {-# SOURCE #-} Plato.Parsing.Lexer
import Plato.Parsing.Monad
import Plato.Parsing.Token

import Plato.Common.Error
import Plato.Common.Location

import Control.Monad.State
import qualified Data.Text as T

----------------------------------------------------------------
-- Layout
-- ref: https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3
----------------------------------------------------------------
layoutKeyword :: Keyword -> Action
layoutKeyword key (pos, _, _, inp) len = do
        sp <- mkSpan pos inp len
        setStartCode layout
        return $ L sp (TokKeyword key)

spaces :: Action -- start code = 0
spaces ainp@(pos, _, _, inp) len = do
        lev <- getIndentLevels
        sp <- mkSpan pos inp 0
        case lev of
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
layoutSpaces (pos@(PsPosn _ _ col), _, _, inp) len = do
        setStartCode code
        lev <- getIndentLevels
        sp <- mkSpan pos inp 0
        let n = col - 1 + len
        case lev of
                _ | T.length inp > len && T.unpack inp !! len == '{' -> do
                        -- note: Layout rule
                        -- If a let, where, do, or of keyword is not followed by the lexeme {,
                        -- the token {n} is inserted after the keyword, where n is the indentation
                        -- of the next lexeme if there is one, or 0 if the end of file has been reached.
                        setStartCode code
                        alexMonadScan
                m : ms | n > m -> do
                        -- note: Layout rule
                        -- L ({n} : ts) (m : ms)   = {  :  (L ts (n : m : ms))         if n > m
                        setIndentLevels (n : m : ms)
                        return $ L sp (TokSymbol SymVLBrace)
                [] | n > 0 -> do
                        -- note: Layout rule
                        -- L ({n} : ts) []         = {  :  (L ts [n])                  if n > 0
                        setIndentLevels [n]
                        return $ L sp (TokSymbol SymVLBrace)
                _ -> do
                        -- note: Layout rule
                        -- L ({n} : ts) ms         = {  :  }  :  (L (< n >: ts) ms)
                        setIndentLevels [n]
                        return $ L sp (TokSymbol SymVLBrace)

rightBrace :: Action
rightBrace (pos, _, _, inp) len = do
        setStartCode code
        sp <- mkSpan pos inp len
        lev <- getIndentLevels
        case lev of
                0 : ms -> do
                        -- note: Layout rule
                        -- L (} : ts) (0 : ms)     = }  :  (L ts ms)
                        setIndentLevels ms
                        return $ L sp (TokSymbol SymRBrace)
                _ -> do
                        -- note: Layout rule
                        -- L (} : ts) ms           = parse-error
                        lift $ throwLocErr sp "missing an opening brace before closing"

leftBrace :: Action
leftBrace (pos, _, _, inp) len = do
        -- note: Layout rule
        -- L ({ : ts) ms           = {  :  (L ts (0 : ms))
        setStartCode code
        sp <- mkSpan pos inp len
        lev <- getIndentLevels
        setIndentLevels (0 : lev)
        return $ L sp (TokSymbol SymLBrace)

popLayoutLevel :: Located Token -> Parser Span
popLayoutLevel (L sp _) = do
        lev <- getIndentLevels
        case lev of
                m : ms | m /= 0 -> do
                        -- note: Layout rule
                        -- L (t : ts) (m : ms)     = }  :  (L (t : ts) ms)             if m≠0 and parse-error(t)
                        setIndentLevels ms
                        return sp
                _ -> lift $ throwLocErr sp "parse error"
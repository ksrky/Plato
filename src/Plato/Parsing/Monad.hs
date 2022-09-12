{-# LANGUAGE MultiParamTypeClasses #-}

module Plato.Parsing.Monad where

import Plato.Parsing.Fixity

import Control.Exception.Safe
import Control.Monad.State.Class
import Control.Monad.Trans
import qualified Data.ByteString.Internal as BS
import Data.Map.Strict as M
import qualified Data.Text as T
import Data.Word

type Parser a = ParserT (Either SomeException) a

----------------------------------------------------------------
-- Basic interface
----------------------------------------------------------------
type AlexInput =
        ( PsPosn -- current position,
        , Char -- previous char
        , [Word8] -- rest of the bytes for the current char
        , T.Text -- current input string
        )

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (pos, c, b : bs, inp) = Just (b, (pos, c, bs, inp))
alexGetByte (pos, _, [], inp) = case T.uncons inp of
        Nothing -> Nothing
        Just (c, inp') -> Just (BS.c2w c, (advancePosn pos c, c, [], inp'))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_, c, _, _) = c

----------------------------------------------------------------
-- Parser position
----------------------------------------------------------------
data PsPosn
        = PsPosn
                !Int -- absolute character offset
                !Int -- line number
                !Int -- column number
        deriving (Eq, Show)

tabsize :: Int
tabsize = 8

advancePosn :: PsPosn -> Char -> PsPosn
advancePosn (PsPosn a l c) '\t' = PsPosn (a + 1) l (c + tabsize - ((c - 1) `mod` tabsize))
advancePosn (PsPosn a l _) '\n' = PsPosn (a + 1) (l + 1) 1
advancePosn (PsPosn a l c) _ = PsPosn (a + 1) l (c + 1)

movePosn :: PsPosn -> T.Text -> Int -> PsPosn
movePosn pos _ 0 = pos
movePosn pos inp len = case T.uncons inp of
        Nothing -> pos
        Just (c, inp') -> movePosn (advancePosn pos c) inp' (len -1)

----------------------------------------------------------------
-- Parser monad
----------------------------------------------------------------
data PsState = PsState
        { parser_pos :: !PsPosn -- position at current input location
        , parser_inp :: T.Text -- the current input
        , parser_chr :: !Char -- the character before the input
        , parser_bytes :: [Word8] -- rest of the bytes for the current char
        , parser_scd :: !Int -- the current startcode
        , parser_ust :: PsUserState -- ParserUserState will be defined in the user program
        }

newtype ParserT m a = ParserT {runParserT :: PsState -> m (a, PsState)}

instance Monad m => Functor (ParserT m) where
        fmap f pa = ParserT $ \s -> do
                (a, s') <- runParserT pa s
                return (f a, s')

instance Monad m => Applicative (ParserT m) where
        pure a = ParserT $ \s -> pure (a, s)
        pf <*> pa = ParserT $ \s -> do
                (f, s') <- runParserT pf s
                (a, s'') <- runParserT pa s'
                return (f a, s'')

instance Monad m => Monad (ParserT m) where
        pa >>= f = ParserT $ \s -> do
                (a, s') <- runParserT pa s
                runParserT (f a) s'

instance Monad m => MonadState PsState (ParserT m) where
        get = ParserT $ \s -> return (s, s)
        put s = ParserT $ \_ -> return ((), s)

instance MonadTrans ParserT where
        lift c = ParserT $ \s -> c >>= (\x -> return (x, s))

parse :: T.Text -> ParserT m a -> m (a, PsState)
parse inp p =
        runParserT
                p
                PsState
                        { parser_pos = startPos
                        , parser_inp = inp
                        , parser_chr = '\n'
                        , parser_bytes = []
                        , parser_scd = 0
                        , parser_ust = initUserState
                        }

startPos :: PsPosn
startPos = PsPosn 0 1 1

getInput :: Monad m => ParserT m AlexInput
getInput =
        ParserT $
                \s@PsState
                        { parser_pos = pos
                        , parser_chr = c
                        , parser_bytes = bs
                        , parser_inp = inp
                        } -> return ((pos, c, bs, inp), s)

setInput :: Monad m => AlexInput -> ParserT m ()
setInput (pos, c, bs, inp) = modify $ \s ->
        s
                { parser_pos = pos
                , parser_chr = c
                , parser_bytes = bs
                , parser_inp = inp
                }

getStartCode :: Monad m => ParserT m Int
getStartCode = gets parser_scd

setStartCode :: Monad m => Int -> ParserT m ()
setStartCode scd = modify $ \s -> s{parser_scd = scd}

----------------------------------------------------------------
-- PsUserState
----------------------------------------------------------------
data PsUserState = PsUserState
        { commentDepth :: Int
        , opDict :: OpDict
        , indentLevels :: [Int]
        }

getUserState :: Monad m => ParserT m PsUserState
getUserState = gets parser_ust

setUserState :: Monad m => PsUserState -> ParserT m ()
setUserState ust = modify $ \s -> s{parser_ust = ust}

initUserState :: PsUserState
initUserState =
        PsUserState
                { commentDepth = 0
                , opDict = M.empty
                , indentLevels = []
                }

getCommentDepth :: Monad m => ParserT m Int
getCommentDepth = commentDepth <$> getUserState

setCommentDepth :: Monad m => Int -> ParserT m ()
setCommentDepth cd = do
        ust <- getUserState
        setUserState ust{commentDepth = cd}

getOpDict :: Monad m => ParserT m OpDict
getOpDict = opDict <$> getUserState

setOpDict :: Monad m => OpDict -> ParserT m ()
setOpDict od = do
        ust <- getUserState
        setUserState ust{opDict = od}

getIndentLevels :: Monad m => ParserT m [Int]
getIndentLevels = indentLevels <$> getUserState

setIndentLevels :: Monad m => [Int] -> ParserT m ()
setIndentLevels il = do
        ust <- getUserState
        setUserState ust{indentLevels = il}
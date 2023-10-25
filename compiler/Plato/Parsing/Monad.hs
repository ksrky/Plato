module Plato.Parsing.Monad where

import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Data.ByteString.Internal  qualified as BS
import Data.IORef
import Data.Text                 qualified as T
import Data.Word

import Plato.Common.Uniq

type Parser a = ParserT IO a

----------------------------------------------------------------
-- Basic interface
----------------------------------------------------------------
type AlexInput =
        ( PsPosn -- current position,
        , Char -- previous char
        , [Word8] -- rest of the bytes for the current char
        , T.Text -- current input text
        )

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (pos, c, b : bs, inp) = Just (b, (pos, c, bs, inp))
alexGetByte (pos, _, [], inp) =
    case T.uncons inp of
        Nothing        -> Nothing
        Just (c, inp') -> Just (BS.c2w c, (advancePosn pos c, c, [], inp'))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_, c, _, _) = c

----------------------------------------------------------------
-- Parser position
----------------------------------------------------------------
data PsPosn = PsPosn !Int !Int !Int
    deriving (Eq, Show)

tabsize :: Int
tabsize = 8

advancePosn :: PsPosn -> Char -> PsPosn
advancePosn (PsPosn a l c) '\t' = PsPosn (a + 1) l (c + tabsize - ((c - 1) `mod` tabsize))
advancePosn (PsPosn a l _) '\n' = PsPosn (a + 1) (l + 1) 1
advancePosn (PsPosn a l c) _    = PsPosn (a + 1) l (c + 1)

movePosn :: PsPosn -> T.Text -> PsPosn
movePosn pos inp = case T.uncons inp of
    Nothing        -> pos
    Just (c, "")   -> advancePosn pos c
    Just (c, inp') -> movePosn (advancePosn pos c) inp'

----------------------------------------------------------------
-- Parser monad
----------------------------------------------------------------
data PsState = PsState
    { -- | file name
      parser_file  :: !FilePath
      -- | position at current input location
    , parser_pos   :: !PsPosn
      -- | the current input
    , parser_inp   :: T.Text
      -- | the character before the input
    , parser_chr   :: !Char
      -- | rest of the bytes for the current char
    , parser_bytes :: [Word8]
      -- | the current startcode
    , parser_scd   :: !Int
      -- | ParserUserState will be defined in the user program
    , parser_ust   :: PsUserState
    }

newtype ParserT m a
    = ParserT { runParserT :: PsState -> m (a, PsState) }

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

instance MonadIO m => MonadIO (ParserT m) where
    liftIO io = ParserT $ \s -> do
        a <- liftIO io
        return (a, s)

parse :: String -> IORef Uniq -> T.Text -> ParserT m a -> m (a, PsState)
parse fn uniq inp parser =
    runParserT parser PsState
                        { parser_file = fn
                        , parser_pos = startPos
                        , parser_inp = inp
                        , parser_chr = '\n'
                        , parser_bytes = []
                        , parser_scd = 0
                        , parser_ust = initUserState uniq
                        }

parseLine :: IORef Uniq -> T.Text -> ParserT m a -> m (a, PsState)
parseLine uniq inp p = parse "<no file name>" uniq inp (ParserT $ \st -> runParserT p st{parser_scd = 1 {-code-}})

startPos :: PsPosn
startPos = PsPosn 0 1 1

getInput :: Monad m => ParserT m AlexInput
getInput = ParserT $ \s@PsState
                        { parser_pos = pos
                        , parser_chr = c
                        , parser_bytes = bs
                        , parser_inp = inp
                        } -> return ((pos, c, bs, inp), s)

setInput :: Monad m => AlexInput -> ParserT m ()
setInput (pos, c, bs, inp) = modify $ \s -> s { parser_pos = pos
                                              , parser_chr = c
                                              , parser_bytes = bs
                                              , parser_inp = inp
                                              }

getFileName :: Monad m => ParserT m FilePath
getFileName = gets parser_file

getStartCode :: Monad m => ParserT m Int
getStartCode = gets parser_scd

setStartCode :: Monad m => Int -> ParserT m ()
setStartCode scd = modify $ \s -> s{parser_scd = scd}

----------------------------------------------------------------
-- PsUserState
----------------------------------------------------------------
data PsUserState = PsUserState
    { ust_commentDepth :: Int
    , ust_indentLevels :: [Int]
    , ust_uniq         :: IORef Uniq
    }

getUserState :: Monad m => ParserT m PsUserState
getUserState = gets parser_ust

setUserState :: Monad m => PsUserState -> ParserT m ()
setUserState ust = modify $ \s -> s{parser_ust = ust}

initUserState :: IORef Uniq -> PsUserState
initUserState ref =
    PsUserState
        { ust_commentDepth = 0
        , ust_indentLevels = []
        , ust_uniq = ref
        }

-- comment depth ------------------------------------------
getCommentDepth :: Monad m => ParserT m Int
getCommentDepth = ust_commentDepth <$> getUserState

setCommentDepth :: Monad m => Int -> ParserT m ()
setCommentDepth cd = do
    ust <- getUserState
    setUserState ust{ust_commentDepth = cd}

-- indent levels ------------------------------------------
getIndentLevels :: Monad m => ParserT m [Int]
getIndentLevels = ust_indentLevels <$> getUserState

setIndentLevels :: Monad m => [Int] -> ParserT m ()
setIndentLevels lev = do
    ust <- getUserState
    setUserState ust{ust_indentLevels = lev}

-- Uniq ------------------------------------------
instance HasUniq PsUserState where
    getUniq = getUniq . ust_uniq
    setUniq uniq ust = setUniq uniq (ust_uniq ust)

instance HasUniq PsState where
    getUniq = getUniq . parser_ust
    setUniq uniq pst = setUniq uniq (parser_ust pst)

freshUniq :: MonadIO m => ParserT m Uniq
freshUniq =  pickUniq =<< get

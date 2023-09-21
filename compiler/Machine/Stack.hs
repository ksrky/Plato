{-# LANGUAGE LambdaCase #-}

module Machine.Stack where

import Control.Monad.Reader
import Data.IORef
import Data.Vector qualified as V

type Ptr = Int

data Cell
        = Int Integer
        | Cons Ptr Ptr

data Reg = S | E | C | D | F

data Stack = Stack
        { cells :: V.Vector (IORef Cell)
        , regS :: IORef Ptr
        , regE :: IORef Ptr
        , regC :: IORef Ptr
        , regD :: IORef Ptr
        , regF :: IORef Ptr
        }

type SECD m = ReaderT Stack m

regSwitch :: Reg -> Stack -> IORef Ptr
regSwitch reg = case reg of
        S -> regS
        E -> regE
        C -> regC
        D -> regD
        F -> regF

readReg :: MonadIO m => Reg -> SECD m Ptr
readReg reg = liftIO . readIORef =<< asks (regSwitch reg)

writeReg :: MonadIO m => Reg -> Ptr -> SECD m ()
writeReg reg p = do
        r <- asks $ regSwitch reg
        liftIO $ writeIORef r p

incrF :: MonadIO m => SECD m ()
incrF = do
        f <- asks regF
        liftIO $ modifyIORef' f (+ 1)

readCell :: MonadIO m => Ptr -> SECD m Cell
readCell p = do
        cells <- asks cells
        liftIO $ readIORef (cells V.! p)

writeCell :: MonadIO m => Ptr -> Cell -> SECD m ()
writeCell p c = do
        cells <- asks cells
        liftIO $ writeIORef (cells V.! p) c

makeInt :: MonadIO m => Integer -> SECD m Ptr
makeInt n = do
        i <- readReg F
        writeCell i (Int n)
        incrF
        return i

makeCons :: MonadIO m => Ptr -> Ptr -> SECD m Ptr
makeCons p1 p2 = do
        i <- readReg F
        writeCell i (Cons p1 p2)
        incrF
        return i

getInt :: (MonadIO m, MonadFail m) => Ptr -> SECD m Integer
getInt p =
        readCell p >>= \case
                Int n -> return n
                _ -> fail "Int required"

getCons :: (MonadIO m, MonadFail m) => Ptr -> SECD m (Ptr, Ptr)
getCons p =
        readCell p >>= \case
                Cons p1 p2 -> return (p1, p2)
                _ -> fail "Cons required"

push :: MonadIO m => Reg -> Ptr -> SECD m ()
push reg i = do
        r <- asks $ regSwitch reg
        liftIO . writeIORef r =<< makeCons i =<< liftIO (readIORef r)

pop :: (MonadIO m, MonadFail m) => Reg -> SECD m Ptr
pop reg = do
        r <- asks $ regSwitch reg
        p <- liftIO $ readIORef r
        (p1, p2) <- getCons p
        liftIO $ writeIORef r p2
        return p1

car :: (MonadIO m, MonadFail m) => Ptr -> ReaderT Stack m Ptr
car i = fst <$> getCons i

cdr :: (MonadIO m, MonadFail m) => Ptr -> ReaderT Stack m Ptr
cdr i = snd <$> getCons i

locate :: forall m. (MonadIO m, MonadFail m) => Ptr -> Ptr -> SECD m Ptr
locate ij r = do
        let loc :: Ptr -> Ptr -> SECD m Ptr
            loc y z = if y == 1 then car z else loc (y - 1) =<< cdr z
        p <- flip loc r =<< car ij
        flip loc p =<< car ij

binOp :: (MonadIO m, MonadFail m) => (Ptr -> Ptr -> SECD m Ptr) -> SECD m ()
binOp op = do
        x <- pop S
        y <- pop S
        push S =<< op x y

rplaca :: (MonadIO m, MonadFail m) => Ptr -> Ptr -> SECD m Ptr
rplaca x y = do
        writeCell x . Cons y =<< cdr x
        return x
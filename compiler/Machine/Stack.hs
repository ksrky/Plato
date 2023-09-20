{-# LANGUAGE LambdaCase #-}

module Machine.Stack where

import Control.Monad.Reader
import Data.IORef
import Data.Vector qualified as V

import Machine.Utils

type Ptr = Int

data Cell
        = Int Integer
        | Cons Ptr Ptr

data Stack = Stack
        { cells :: V.Vector (IORef Cell)
        , regS :: IORef Ptr
        , regE :: IORef Ptr
        , regC :: IORef Ptr
        , regD :: IORef Ptr
        , regF :: IORef Ptr
        }

type SECD m = ReaderT Stack m

getS :: MonadIO m => SECD m Ptr
getS = readMIORef =<< asks regS

getE :: MonadIO m => SECD m Ptr
getE = readMIORef =<< asks regE

getC :: MonadIO m => SECD m Ptr
getC = readMIORef =<< asks regC

writeC :: MonadIO m => Ptr -> SECD m ()
writeC p = do
        c <- asks regC
        writeMIORef c p

getD :: MonadIO m => SECD m Ptr
getD = readMIORef =<< asks regD

getF :: MonadIO m => SECD m Ptr
getF = readMIORef =<< asks regF

incrF :: MonadIO m => SECD m ()
incrF = do
        f <- asks regF
        modifyMIORef f (+ 1)

writeCell :: MonadIO m => Ptr -> Cell -> SECD m ()
writeCell p c = do
        cells <- asks cells
        writeMIORef (cells V.! p) c

readCell :: MonadIO m => Ptr -> SECD m Cell
readCell p = do
        cells <- asks cells
        readMIORef (cells V.! p)

makeInt :: MonadIO m => Integer -> SECD m Ptr
makeInt n = do
        i <- getF
        writeCell i (Int n)
        incrF
        return i

makeCons :: MonadIO m => Ptr -> Ptr -> SECD m Ptr
makeCons p1 p2 = do
        i <- getF
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

push :: MonadIO m => Ptr -> IORef Ptr -> SECD m ()
push i r = liftIO . writeIORef r =<< makeCons i =<< readMIORef r

pop :: (MonadIO m, MonadFail m) => IORef Ptr -> SECD m Ptr
pop r = do
        p <- readMIORef r
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

binOp :: (MonadIO m, MonadFail m) => IORef Ptr -> (Ptr -> Ptr -> SECD m Ptr) -> SECD m ()
binOp r op = do
        x <- pop r
        y <- pop r
        flip push r =<< op x y
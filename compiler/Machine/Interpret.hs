module Machine.Interpret where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Machine.Stack
import Prelude hiding (EQ, GT, LT)

data Instr
        = ---- Stop ----
          STOP
        | ---- Loading ----
          LDC Integer
        | LD Ptr Ptr
        | ---- List operations ----
          NIL
        | CAR
        | CDR
        | CONS
        | ---- Arithmetic operators ----
          ADD
        | SUB
        | MUL
        | DIV
        | REM
        | ---- Relational operators ----
          EQ
        | NE
        | LT
        | LE
        | GT
        | GE
        | ---- Control ----
          SEL [Instr] [Instr]
        | JOIN
        | ---- Function ----
          LDF [Instr]
        | RTN
        | AP
        | DUM
        | RAP
        | ---- I/O ----
          READ
        | PRINT
        deriving (Eq, Show)

commandId :: Instr -> Integer
commandId STOP = 0
commandId LDC{} = 1
commandId LD{} = 2
commandId NIL = 3
commandId CAR = 4
commandId CDR = 5
commandId CONS = 6
commandId ADD = 7
commandId SUB = 8
commandId MUL = 9
commandId DIV = 10
commandId REM = 11
commandId EQ = 12
commandId NE = 13
commandId LT = 14
commandId LE = 15
commandId GT = 16
commandId GE = 17
commandId SEL{} = 21
commandId JOIN = 22
commandId LDF{} = 23
commandId RTN = 24
commandId AP = 25
commandId DUM = 26
commandId RAP = 27
commandId READ = 28
commandId PRINT = 29

loadCommand :: MonadIO m => Ptr -> Integer -> SECD m Ptr
loadCommand i n = do
        j <- makeInt n
        makeCons j i

loadInstr :: MonadIO m => Ptr -> Instr -> SECD m Ptr
loadInstr i instr = do
        j <- loadInstr' instr
        loadCommand j (commandId instr)
    where
        loadInstr' (LDC n) = flip makeCons i =<< makeInt n
        loadInstr' (LD x y) = flip makeCons i =<< makeCons x y
        loadInstr' (SEL ts fs) = do
                cf <- flip makeCons i =<< loadInstrs fs
                flip makeCons cf =<< loadInstrs ts
        loadInstr' (LDF es) = flip makeCons i =<< loadInstrs es
        loadInstr' _ = return i

loadInstrs :: MonadIO m => [Instr] -> SECD m Ptr
loadInstrs = foldM loadInstr 0 . reverse

runCommand :: (MonadIO m, MonadFail m) => SECD m ()
runCommand = do
        n <- getInt =<< getC
        case n of
                0 {- STOP -} -> return ()
                1 {- LDC -} -> do
                        x <- pop =<< asks regC
                        push x =<< asks regS
                2 {- LD -} -> do
                        ij <- pop =<< asks regC
                        p <- locate ij =<< getE
                        push p =<< asks regS
                3 {- NIL -} -> do
                        i <- makeInt 0
                        push i =<< asks regS
                4 {- CAR -} -> do
                        p <- car =<< pop =<< asks regS
                        push p =<< asks regS
                5 {- CDR -} -> do
                        p <- cdr =<< pop =<< asks regS
                        push p =<< asks regS
                6 {- CONS -} -> do
                        p2 <- pop =<< asks regS
                        p1 <- pop =<< asks regS
                        p <- makeCons p1 p2
                        push p =<< asks regS
                7 {- ADD -} -> arithOp (+)
                8 {- SUB -} -> arithOp (-)
                9 {- MUL -} -> arithOp (*)
                10 {- DIV -} -> arithOp div
                11 {- REM -} -> arithOp rem
                12 {- EQ -} -> relOp (==)
                13 {- NE -} -> relOp (/=)
                14 {- LT -} -> relOp (<)
                15 {- LE -} -> relOp (<=)
                16 {- GT -} -> relOp (>)
                17 {- GE -} -> relOp (>=)
                21 {- SEL -} -> do
                        b <- getInt =<< getS
                        ct <- pop =<< asks regC
                        cf <- pop =<< asks regC
                        c <- getC
                        push c =<< asks regD
                        writeC $ if b == 0 then ct else cf
                _ -> fail "Invalid operation"
    where
        bool2integer :: Bool -> Integer
        bool2integer True = 1
        bool2integer False = 0
        arithOp :: (MonadIO m, MonadFail m) => (Integer -> Integer -> Integer) -> SECD m ()
        arithOp op = do
                s <- asks regS
                binOp s (\x y -> makeInt =<< op <$> getInt x <*> getInt y)
        relOp :: (MonadIO m, MonadFail m) => (Integer -> Integer -> Bool) -> SECD m ()
        relOp op = do
                s <- asks regS
                binOp s (\x y -> (makeInt . bool2integer) =<< (op <$> getInt x <*> getInt y))
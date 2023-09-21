module Machine.Interpret where

import Control.Monad
import Control.Monad.IO.Class
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
        n <- getInt =<< readReg C
        case n of
                0 {- STOP -} -> writeReg C 0
                1 {- LDC -} -> do
                        push S =<< pop C
                2 {- LD -} -> do
                        ij <- pop C
                        push S =<< locate ij =<< readReg E
                3 {- NIL -} -> push S =<< makeInt 0
                4 {- CAR -} -> push S =<< car =<< pop S
                5 {- CDR -} -> push S =<< cdr =<< pop S
                6 {- CONS -} -> do
                        p2 <- pop S
                        p1 <- pop S
                        push S =<< makeCons p1 p2
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
                        b <- getInt =<< readReg S
                        ct <- pop C
                        cf <- pop C
                        push D =<< readReg C
                        writeReg C $ if b == 0 then ct else cf
                22 {- JOIN -} -> writeReg C =<< pop D
                23 {- LDF -} -> do
                        f <- pop C
                        fe <- makeCons f =<< readReg E
                        push S fe
                24 {- RTN -} -> do
                        x <- pop S
                        writeReg S =<< pop D
                        push S x
                        writeReg E =<< pop D
                        writeReg C =<< pop D
                25 {- AP -} -> do
                        (f, e') <- getCons =<< pop S
                        v <- pop S
                        push D =<< readReg C
                        writeReg C f
                        push D =<< readReg E
                        writeReg E e'
                        push E v
                        push D =<< readReg S
                        writeReg S 0
                26 {- DUM -} -> push E 0
                27 {- RAP -} -> do
                        (f, ne) <- getCons =<< pop S
                        v <- pop S
                        push D =<< readReg C
                        writeReg C f
                        push D =<< cdr =<< readReg E
                        writeReg E =<< rplaca ne v
                        push D =<< readReg S
                        writeReg S 0
                28 {- READ -} -> do
                        i <- makeInt =<< liftIO readLn
                        push S i
                29 {- PRINT -} -> do
                        n <- getInt =<< pop S
                        liftIO $ print n
                _ -> fail "Invalid operation"
    where
        bool2integer :: Bool -> Integer
        bool2integer True = 1
        bool2integer False = 0
        arithOp :: (MonadIO m, MonadFail m) => (Integer -> Integer -> Integer) -> SECD m ()
        arithOp op = binOp (\x y -> makeInt =<< op <$> getInt x <*> getInt y)
        relOp :: (MonadIO m, MonadFail m) => (Integer -> Integer -> Bool) -> SECD m ()
        relOp op = binOp (\x y -> (makeInt . bool2integer) =<< (op <$> getInt x <*> getInt y))
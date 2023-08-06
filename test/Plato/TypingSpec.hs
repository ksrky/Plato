{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.TypingSpec where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.IORef
import Data.Text qualified as T
import Prettyprinter
import Test.Hspec

import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Parsing
import Plato.PsToTyp
import Plato.PsToTyp.Scoping
import Plato.Typing
import Plato.Typing.Env

spec :: Spec
spec = do
        describe "Type checking of declarations" $ do
                it "lambda abstraction" $ do
                        test_decls "id : {a} a -> a; id = \\x -> x" `shouldReturn` ()
                it "function clause" $ do
                        test_decls "id : {a} a -> a; id x = x" `shouldReturn` ()
        {-
        describe "elaboration of pattern matching" $ do
                it "1 constructor-pattern" $ do
                        test_clauses "not : Bool -> Bool; not True = False; not False = True"
                                `shouldReturn` [ "not : Bool -> Bool"
                                               , "not = \\$:Bool. case $ of {True -> False; False -> True}"
                                               ]
                it "1 constructor-pattern and 1 variable-pattern" $ do
                        test_clauses "(+) : Nat -> Nat -> Nat; Zero + n = n; Succ m + n = Succ (m + n)"
                                `shouldReturn` [ "+ : Nat -> Nat -> Nat"
                                               , "\\$:Nat. \\$:Nat. case $ of {Zero -> $; Succ $ -> Succ (+ $ $)}"
                                               ]
                it "2 constructor-patterns" $ do
                        test_clauses "(<) : Nat -> Nat -> Bool; Succ m < Succ n = m < n; Zero < Succ _ = True; _ < Zero = False"
                                `shouldReturn` [ "< : Nat -> Nat -> Nat"
                                               , "< = \\$:Nat. \\$:Nat. case $ of {Zero -> case $ of {Zero -> False; Succ $ -> True}; Succ $ -> case $ of {Zero -> False; Succ $ -> < $ $}}"
                                               ]
        -}
        describe "Type checking of a file" $ do
                it "test01.pla" $ do
                        test_file "test01.pla" `shouldReturn` ()
                it "test02.pla" $ do
                        test_file "test02.pla" `shouldReturn` ()
                it "test03.pla" $ do
                        test_file "test03.pla" `shouldReturn` ()
                it "test04.pla" $ do
                        test_file "test04.pla" `shouldReturn` ()
                it "test05.pla" $ do
                        test_file "test05.pla" `shouldReturn` ()
                it "test06.pla" $ do
                        test_file "test06.pla" `shouldReturn` ()
                it "test07.pla" $ do
                        test_file "test07.pla" `shouldReturn` ()
                it "test08.pla" $ do
                        test_file "test08.pla" `shouldReturn` ()
                it "test09.pla" $ do
                        test_file "test09.pla" `shouldReturn` ()
                it "test10.pla" $ do
                        test_file "test10.pla" `shouldReturn` ()
                it "test15.pla" $ do
                        test_file "test15.pla" `shouldReturn` ()
                it "test16.pla" $ do
                        test_file "test16.pla" `shouldReturn` ()
        describe "Uniq rewrited?" $ do
                it "test01.pla" $ do
                        test_uniq "test01.pla" >>= (`shouldSatisfy` isSorted)
                it "test02.pla" $ do
                        test_uniq "test02.pla" >>= (`shouldSatisfy` isSorted)

data Context = Context
        { ctx_uniq :: IORef Uniq
        , ctx_scope :: Scope
        , ctx_typEnv :: TypEnv
        , ctx_conEnv :: ConEnv
        }

instance HasUniq Context where
        getUniq = return . ctx_uniq
        setUniq uniq ref = setUniq uniq (ctx_uniq ref)

instance HasScope Context where
        getScope = ctx_scope
        modifyScope f ctx = ctx{ctx_scope = f (ctx_scope ctx)}

instance HasTypEnv Context where
        getTypEnv = ctx_typEnv
        modifyTypEnv f ctx = ctx{ctx_typEnv = f (ctx_typEnv ctx)}

instance HasConEnv Context where
        getConEnv = ctx_conEnv
        modifyConEnv f ctx = ctx{ctx_conEnv = f (ctx_conEnv ctx)}

test_decls :: T.Text -> IO ()
test_decls inp = do
        uref <- initUniq
        let ctx = Context uref initScope initTypEnv initConEnv
        decs <- runReaderT (parseDecls inp) ctx
        decs' <- runReaderT (execWriterT $ elabDecls decs) ctx
        void $ runReaderT (typingDecls decs') ctx

test_clauses :: T.Text -> IO [String]
test_clauses inp = do
        uref <- initUniq
        let ctx = Context uref initScope initTypEnv initConEnv
        decs <- runReaderT (parseDecls inp) ctx
        decs' <- runReaderT (execWriterT $ elabDecls decs) ctx
        (decs'', _) <- runReaderT (typingDecls decs') ctx
        return $ map (show . pretty) decs''

test_file :: FilePath -> IO ()
test_file fn =
        runReaderT
                ( do
                        pssyn <- parseFile ("test/testcases/" ++ fn)
                        pssyn' <- nicify pssyn
                        typsyn <- psToTyp pssyn'
                        typsyn' <- typing typsyn
                        liftIO $ print $ map (show . pretty) typsyn'
                )
                =<< initSession

test_uniq :: String -> IO [Uniq]
test_uniq fn =
        runReaderT
                ( do
                        pssyn <- parseFile ("test/testcases/" ++ fn)
                        uniq1 <- liftIO . readIORef =<< getUniq =<< ask
                        pssyn' <- nicify pssyn
                        uniq2 <- liftIO . readIORef =<< getUniq =<< ask
                        typsyn <- psToTyp pssyn'
                        uniq3 <- liftIO . readIORef =<< getUniq =<< ask
                        _ <- typing typsyn
                        uniq4 <- liftIO . readIORef =<< getUniq =<< ask
                        liftIO $ print [uniq1, uniq2, uniq3, uniq4]
                        return [uniq1, uniq2, uniq3, uniq4]
                )
                =<< initSession

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : xs) = x <= y && isSorted (y : xs)
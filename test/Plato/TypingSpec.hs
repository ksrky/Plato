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
import Plato.PsToTyp.Graph
import Plato.Typing
import Plato.Typing.Env

spec :: Spec
spec = do
        describe "Type checking of declarations" $ do
                it "lambda abstraction" $ do
                        test_defns "id : {a} a -> a; id = \\x -> x" `shouldReturn` ()
                it "function clause" $ do
                        test_defns "id : {a} a -> a; id x = x" `shouldReturn` ()
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
                it "01.pla" $ do
                        test_file "01.pla" `shouldReturn` ()
                it "02.pla" $ do
                        test_file "02.pla" `shouldReturn` ()
                it "03.pla" $ do
                        test_file "03.pla" `shouldReturn` ()
                it "04.pla" $ do
                        test_file "04.pla" `shouldReturn` ()
                it "05.pla" $ do
                        test_file "05.pla" `shouldReturn` ()
                it "06.pla" $ do
                        test_file "06.pla" `shouldReturn` ()
                it "07.pla" $ do
                        test_file "07.pla" `shouldReturn` ()
                it "08.pla" $ do
                        test_file "08.pla" `shouldReturn` ()
                it "09.pla" $ do
                        test_file "09.pla" `shouldReturn` ()
                it "10.pla" $ do
                        test_file "10.pla" `shouldReturn` ()
                it "15.pla" $ do
                        test_file "15.pla" `shouldReturn` ()
                it "16.pla" $ do
                        test_file "16.pla" `shouldReturn` ()
        describe "Uniq rewrited?" $ do
                it "01.pla" $ do
                        test_uniq "01.pla" >>= (`shouldSatisfy` isSorted)
                it "02.pla" $ do
                        test_uniq "02.pla" >>= (`shouldSatisfy` isSorted)

data Context = Context
        { ctx_uniq :: IORef Uniq
        , ctx_scope :: Scope
        , ctx_typEnv :: TypEnv
        , ctx_conEnv :: ConEnv
        }

instance HasUniq Context where
        getUniq = return . ctx_uniq
        setUniq uniq ref = setUniq uniq (ctx_uniq ref)

instance HasTypEnv Context where
        getTypEnv = ctx_typEnv
        modifyTypEnv f ctx = ctx{ctx_typEnv = f (ctx_typEnv ctx)}

instance HasConEnv Context where
        getConEnv = ctx_conEnv
        modifyConEnv f ctx = ctx{ctx_conEnv = f (ctx_conEnv ctx)}

test_defns :: T.Text -> IO ()
test_defns inp = do
        uref <- initUniq
        let ctx = Context uref mempty mempty mempty
        decs <- runReaderT (parseDecls inp) ctx
        scg <- initScopeGraph
        defs <- runReaderT (execWriterT $ mapM (elabDecl scg) decs) ctx
        void $ runReaderT (runWriterT $ typingDefns defs) ctx

test_clauses :: T.Text -> IO [String]
test_clauses inp = do
        uref <- initUniq
        let ctx = Context uref mempty mempty mempty
        decs <- runReaderT (parseDecls inp) ctx
        scg <- initScopeGraph
        defs <- runReaderT (execWriterT $ mapM (elabDecl scg) decs) ctx
        (_, defs') <- runReaderT (runWriterT $ typingDefns defs) ctx
        return $ map (show . pretty) defs'

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
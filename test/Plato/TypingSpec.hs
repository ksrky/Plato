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
        describe "Type checking of a file" $ do
                it "test01.plt" $ do
                        test_file "test01.plt" `shouldReturn` ()
                it "test02.plt" $ do
                        test_file "test02.plt" `shouldReturn` ()
                it "test03.plt" $ do
                        test_file "test03.plt" `shouldReturn` ()
                it "test04.plt" $ do
                        test_file "test04.plt" `shouldReturn` ()
                it "test05.plt" $ do
                        test_file "test05.plt" `shouldReturn` ()
                it "test06.plt" $ do
                        test_file "test06.plt" `shouldReturn` ()
                it "test07.plt" $ do
                        test_file "test07.plt" `shouldReturn` ()
                it "test08.plt" $ do
                        test_file "test08.plt" `shouldReturn` ()
                it "test09.plt" $ do
                        test_file "test09.plt" `shouldReturn` ()
                it "test10.plt" $ do
                        test_file "test10.plt" `shouldReturn` ()
                it "test15.plt" $ do
                        test_file "test15.plt" `shouldReturn` ()
                it "test16.plt" $ do
                        test_file "test16.plt" `shouldReturn` ()
        describe "Uniq rewrited?" $ do
                it "test01.plt" $ do
                        test_uniq "test01.plt" >>= (`shouldSatisfy` isSorted)
                it "test02.plt" $ do
                        test_uniq "test02.plt" >>= (`shouldSatisfy` isSorted)

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
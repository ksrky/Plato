{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.TypingSpec where

import Control.Monad.Reader
import Data.IORef
import Data.Text qualified as T
import Test.Hspec

import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Parsing
import Plato.Parsing.Parser
import Plato.PsToTyp
import Plato.PsToTyp.Scoping
import Plato.Typing
import Plato.Typing.Monad

spec :: Spec
spec = do
        describe "Type checking of declarations" $ do
                it "lambda abstraction" $ do
                        test_decls "id : {a} a -> a; id = \\x -> x" `shouldReturn` ()
        describe "Type checking of a file" $ do
                it "test01.plt" $ do
                        test_file "test01.plt" `shouldReturn` ()

data Context = Context {ctx_uniq :: IORef Uniq, ctx_scope :: Scope}

instance HasUniq Context where
        getUniq = return . ctx_uniq

instance HasScope Context where
        getScope (Context _ sc) = sc
        modifyScope f ctx = ctx{ctx_scope = f (ctx_scope ctx)}

test_decls :: T.Text -> IO ()
test_decls inp = do
        uniq <- initUniq
        decs <- runReaderT (parsePartial inp declsParser) uniq
        decs' <- runReaderT (elabDecls $ map unLoc decs) (Context uniq initScope)
        ctx <- runReaderT initContext uniq
        void $ runReaderT (typingDecls decs') ctx

test_file :: FilePath -> IO ()
test_file fn =
        runReaderT
                ( do
                        pssyn <- parseFile ("test/testcases/" ++ fn)
                        pssyn' <- nicify pssyn
                        typsyn' <- ps2typ pssyn'
                        void $ typing typsyn'
                )
                =<< initSession

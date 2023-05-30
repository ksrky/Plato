{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.TypToCoreSpec where

import Control.Monad.Reader
import Data.IORef
import Data.Text qualified as T
import Prettyprinter
import Test.Hspec

import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Core.Env
import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Parsing
import Plato.Parsing.Parser
import Plato.PsToTyp qualified as T
import Plato.PsToTyp.Scoping
import Plato.TypToCore qualified as C
import Plato.Typing
import Plato.Typing.Monad

spec :: Spec
spec = do
        describe "Core elaboration of declarations" $ do
                it "lambda abstraction" $ do
                        test_decls "id : {a} a -> a; id = \\x -> x"
                                `shouldReturn` [ "? = fix (λ?:{id:∀a*. a->a}. {id=Λa:*. (Λa:*. λx:a. x)a})"
                                               , "id = ?.0"
                                               ]
                it "function clause" $ do
                        test_decls "id : {a} a -> a; id x = x"
                                `shouldReturn` [ "? = fix (λ?:{id:∀a*. a->a}. {id=Λa:*. (Λa:*. λ$9:a. $9)a})"
                                               , "id = ?.0"
                                               ]
        describe "Core elaboration of a file" $ do
                it "test01.plt" $ do
                        test_file "test01.plt"
                                `shouldReturn` [ "Bool = μBool:*. ({}|{})"
                                               , "True = (fold [Bool]) inj_0[Bool]()"
                                               , "False = (fold [Bool]) inj_1[Bool]()"
                                               ]
                it "test02.plt" $ do
                        test_file "test02.plt"
                                `shouldReturn` [ "? = fix (λ?:{id:∀a*. a->a}. {id=Λa:*. (Λa:*. λx:a. x)a})"
                                               , "id = ?.0"
                                               ]
                it "test03.plt" $ do
                        test_file "test03.plt"
                                `shouldReturn` [ "Nat = μNat:*. ({}|{?:Nat})"
                                               , "Zero = (fold [Nat]) inj_0[Nat]()"
                                               , "Succ = (fold [Nat]) (λ?:Nat. inj_1[Nat](?))"
                                               ]
                it "test04.plt" $ do
                        test_file "test04.plt"
                                `shouldReturn` [ "? = fix (λ?:{g:∀a*. ∀b*. (a->b)->a->b}. {g=Λa:*. Λb:*. ((Λa:*. Λb:*. λ$17:a. λ$16:a->b. $16 $17)a)b})"
                                               , "g = ?.0"
                                               ]
                it "test05.plt" $ do
                        test_file "test05.plt"
                                `shouldReturn` [ "Bool = μBool:*. ({}|{})"
                                               , "True = (fold [Bool]) inj_0[Bool]()"
                                               , "False = (fold [Bool]) inj_1[Bool]()"
                                               , "? = fix (λ?:{not:Bool->Bool}. {not=λ$16:Bool. case ((unfold [Bool]) $16) {True -> False | False -> True}})"
                                               , "not = ?.0"
                                               ]

data Context = Context {ctx_uniq :: IORef Uniq, ctx_scope :: Scope}

instance HasUniq Context where
        getUniq = return . ctx_uniq

instance HasScope Context where
        getScope (Context _ sc) = sc
        modifyScope f ctx = ctx{ctx_scope = f (ctx_scope ctx)}

test_decls :: T.Text -> IO [String]
test_decls inp = do
        uniq <- initUniq
        decs <- runReaderT (parsePartial inp declsParser) uniq
        decs' <- runReaderT (T.elabDecls $ map unLoc decs) (Context uniq initScope)
        ctx <- runReaderT initContext uniq
        decs'' <- runReaderT (typingDecls decs') ctx
        cmds <- runReaderT (C.elabDecls decs'') initCoreEnv
        return $ map (show . pretty) cmds

test_file :: FilePath -> IO [String]
test_file fn =
        runReaderT
                ( do
                        pssyn <- parseFile ("test/testcases/" ++ fn)
                        pssyn' <- nicify pssyn
                        typsyn <- T.ps2typ pssyn'
                        typsyn' <- typing typsyn
                        coresyn <- C.typ2core typsyn'
                        return $ map (show . pretty) coresyn
                )
                =<< initSession
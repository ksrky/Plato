{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.TypToCoreSpec where

import Test.Hspec

spec :: Spec
spec = return ()

{-
import Control.Monad.Reader
import Data.IORef
import Data.Text qualified as T
import Prettyprinter
import Test.Hspec

import Plato.Common.Uniq
import Plato.Core.Env
import Plato.Core.Monad
import Plato.Driver.Monad
import Plato.Nicifier
import Plato.Parsing
import Plato.Parsing.Parser
import Plato.PsToTyp qualified as T
import Plato.PsToTyp.Scoping
import Plato.RunCore
import Plato.TypToCore qualified as C
import Plato.Typing
import Plato.Typing.Monad

spec :: Spec
spec = do
        describe "Core elaboration of declarations" $ do
                it "lambda abstraction" $ do
                        test_decls "id : {a} a -> a; id = \\x -> x"
                                `shouldReturn` [ "? = fix (λ?:{id:∀a*. a->a}. {id=(λid:∀a*. a->a. Λa:*. (Λa:*. λx:a. x) a) (?.0)})"
                                               , "id = ?.0"
                                               ]
                it "function clause" $ do
                        test_decls "id : {a} a -> a; id x = x"
                                `shouldReturn` [ "? = fix (λ?:{id:∀a*. a->a}. {id=(λid:∀a*. a->a. Λa:*. (Λa:*. λ$9:a. $9) a) (?.0)})"
                                               , "id = ?.0"
                                               ]
        describe "Core elaboration of a file" $ do
                it "test01.plt" $ do
                        test_file "test01.plt"
                                `shouldReturn` [ "Bool = μBool:*. {}|{}"
                                               , "True = (fold [Bool]) inj_0[{}|{}]()"
                                               , "False = (fold [Bool]) inj_1[{}|{}]()"
                                               ]
                it "test02.plt" $ do
                        test_file "test02.plt"
                                `shouldReturn` [ "? = fix (λ?:{id:∀a*. a->a}. {id=(λid:∀a*. a->a. Λa:*. (Λa:*. λx:a. x) a) (?.0)})"
                                               , "id = ?.0"
                                               ]
                it "test03.plt" $ do
                        test_file "test03.plt"
                                `shouldReturn` [ "Nat = μNat:*. {}|{?:Nat}"
                                               , "Zero = (fold [Nat]) inj_0[{}|{?:Nat}]()"
                                               , "Succ = λ?:Nat. (fold [Nat]) (inj_1[{}|{?:Nat}](?))"
                                               ]
                it "test04.plt" $ do
                        test_file "test04.plt"
                                `shouldReturn` [ "? = fix (λ?:{g:∀a*. ∀b*. (a->b)->a->b}. {g=(λg:∀a*. ∀b*. (a->b)->a->b. Λa:*. Λb:*. ((Λa:*. Λb:*. λ$17:a. λ$16:a->b. $16 $17) a) b) (?.0)})"
                                               , "g = ?.0"
                                               ]
                it "test05.plt" $ do
                        test_file "test05.plt"
                                `shouldReturn` [ "Bool = μBool:*. {}|{}"
                                               , "True = (fold [Bool]) inj_0[{}|{}]()"
                                               , "False = (fold [Bool]) inj_1[{}|{}]()"
                                               , "? = fix (λ?:{not:Bool->Bool}. {not=(λnot:Bool->Bool. λ$16:Bool. case ((unfold [Bool]) $16) {True -> False | False -> True}) (?.0)})"
                                               , "not = ?.0"
                                               ]
                it "test06.plt" $ do
                        test_file "test06.plt"
                                `shouldReturn` [ "? = fix (λ?:{f:∀a*. a->a}. {f=(λf:∀a*. a->a. Λa:*. (Λa:*. (λ?:{g:∀b*. b->b, h:∀c*. c->c}. (λh:∀c*. c->c. (λg:∀b*. b->b. g a) (?.1)) (?.0)) (fix (λ?:{g:∀b*. b->b, h:∀c*. c->c}. {g=(λg:∀b*. b->b. Λb:*. (Λb:*. λx:b. x) b) (?.0),h=(λh:∀c*. c->c. Λc:*. (Λc:*. λy:c. y) c) (?.1)}))) a) (?.0)})"
                                               , "f = ?.0"
                                               ]
                it "test07.plt" $ do
                        test_file "test07.plt"
                                `shouldReturn` [ "Bool = μBool:*. {}|{}"
                                               , "True = (fold [Bool]) inj_0[{}|{}]()"
                                               , "False = (fold [Bool]) inj_1[{}|{}]()"
                                               , "? = fix (λ?:{not:Bool->Bool}. {not=(λnot:Bool->Bool. λ$15:Bool. case ((unfold [Bool]) $15) {True -> False | False -> True}) (?.0)})"
                                               , "not = ?.0"
                                               ]

data Context = Context {ctx_uniq :: IORef Uniq, ctx_scope :: Scope}

instance HasUniq Context where
        getUniq = return . ctx_uniq
        setUniq uniq ref = setUniq uniq (ctx_uniq ref)

instance HasScope Context where
        getScope (Context _ sc) = sc
        modifyScope f ctx = ctx{ctx_scope = f (ctx_scope ctx)}

test_decls :: T.Text -> IO [String]
test_decls inp = do
        uniq <- initUniq
        decs <- runReaderT (parsePartial inp declsParser) uniq
        decs' <- runReaderT (T.elabDecls  decs) (Context uniq initScope)
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
                        typsyn <- T.psToTyp pssyn'
                        typsyn' <- typing typsyn
                        coresyn <- C.typToCore typsyn'
                        liftIO $ unCore (checkCommands coresyn) initCoreEnv
                        return $ map (show . pretty) coresyn
                )
                =<< initSession-}
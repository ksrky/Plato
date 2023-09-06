{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.TypToCoreSpec where

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
import Plato.PsToTyp qualified as T
import Plato.TypToCore qualified as C
import Plato.Typing
import Plato.Typing.Env

spec :: Spec
spec = do
        describe "Core elaboration of declarations" $ do
                it "lambda abstraction" $ do
                        test_decls "id : {a} a -> a; id = \\x -> x"
                                `shouldReturn` [ "id_4 : (a_1 : Type) -> a_1 -> a_1"
                                               , "id_4 = \\ a_8 : Type . \\ x_5 : a_8 . x_5"
                                               ]
                it "function clause" $ do
                        test_decls "id : {a} a -> a; id x = x"
                                `shouldReturn` [ "id_4 : (a_1 : Type) -> a_1 -> a_1"
                                               , "id_4 = \\ a_8 : Type . \\ ?_9 : a_8 . ?_9"
                                               ]
        describe "Core elaboration of a file" $ do
                it "01.pla" $ do
                        test_file "01.pla"
                                `shouldReturn` [ "Bool_0 : Type"
                                               , "Bool_0 = (l_7 : {True, False}) * case l_6 of {True -> {unit}; False -> {unit}}"
                                               , "True_1 : Bool_0"
                                               , "False_3 : Bool_0"
                                               , "True_1 = (`True, `unit)"
                                               , "False_3 = (`False, `unit)"
                                               ]
                it "02.pla" $ do
                        test_file "02.pla"
                                `shouldReturn` [ "id_4 : (a_1 : Type) -> a_1 -> a_1"
                                               , "id_4 = \\ a_8 : Type . \\ x_5 : a_8 . x_5"
                                               ]
                it "03.pla" $ do
                        test_file "03.pla"
                                `shouldReturn` [ "Nat_0 : Type"
                                               , "Nat_0 = (l_8 : {Zero, Succ}) * case l_7 of {Zero -> {unit}; Succ -> Rec [Nat_0]}"
                                               , "Zero_1 : Nat_0"
                                               , "Succ_3 : Nat_0 -> Nat_0"
                                               , "Zero_1 = (`Zero, `unit)"
                                               , "Succ_3 = \\ 0_10 : Nat_0 . (`Succ, fold 0_10)"
                                               ]
                it "04.pla" $ do
                        test_file "04.pla"
                                `shouldReturn` [ "g_7 : (a_1 : Type) -> (b_2 : Type) -> (a_1 -> b_2) -> a_1 -> b_2"
                                               , "g_7 = \\ a_14 : Type . \\ b_15 : Type . \\ ?_16 : (a_14 -> b_15) . \\ ?_17 : a_14 . ?_16 ?_17"
                                               ]
                it "05.pla" $ do
                        test_file "05.pla"
                                `shouldReturn` [ "Bool_0 : Type"
                                               , "Bool_0 = (l_19 : {True, False}) * case l_18 of {True -> {unit}; False -> {unit}}"
                                               , "True_1 : Bool_0"
                                               , "False_3 : Bool_0"
                                               , "True_1 = (`True, `unit)"
                                               , "False_3 = (`False, `unit)"
                                               , "not_8 : Bool_0 -> Bool_0"
                                               , "not_8 = \\ ?_17 : Bool_0 . (\\ ?_16 : Bool_0 . split ?_16 with (x_21, y_22) -> !case x_21 of {True -> [False_3]; False -> [True_1]}) ?_17"
                                               ]
                it "06.pla" $ do
                        test_file "06.pla"
                                `shouldReturn` [ "f_4 : (a_1 : Type) -> a_1 -> a_1"
                                               , "f_4 = \\ a_23 : Type . let {g_9 : (b_6 : Type) -> b_6 -> b_6; h_16 : (c_13 : Type) -> c_13 -> c_13; g_9 = \\ b_24 : Type . \\ x_10 : b_24 . x_10; h_16 = \\ c_25 : Type . \\ y_17 : c_25 . y_17} in g_9 a_23"
                                               ]
                it "07.pla" $ do
                        test_file "07.pla"
                                `shouldReturn` [ "Bool_0 : Type"
                                               , "Bool_0 = (l_17 : {True, False}) * case l_16 of {True -> {unit}; False -> {unit}}"
                                               , "True_1 : Bool_0"
                                               , "False_3 : Bool_0"
                                               , "True_1 = (`True, `unit)"
                                               , "False_3 = (`False, `unit)"
                                               , "not_8 : Bool_0 -> Bool_0"
                                               , "not_8 = \\ ?_15 : Bool_0 . split ?_15 with (x_19, y_20) -> !case x_19 of {True -> [False_3]; False -> [True_1]}"
                                               ]

data Context = Context
        { ctx_uniq :: IORef Uniq
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

test_decls :: T.Text -> IO [String]
test_decls inp = do
        uref <- initUniq
        let ctx = Context uref mempty mempty
        decs <- runReaderT (parseDecls inp) ctx
        defs <- runReaderT (T.elabTopDecls mempty decs) ctx
        (ctx', defs') <- runReaderT (runWriterT (typingDefns defs)) ctx
        prog <- concat <$> runReaderT (mapM C.elabDefn defs') ctx'
        return $ map (show . pretty) prog

test_file :: FilePath -> IO [String]
test_file fn =
        runReaderT
                ( do
                        pssyn <- parseFile ("test/testcases/" ++ fn)
                        pssyn' <- nicify pssyn
                        typsyn <- T.psToTyp pssyn'
                        typsyn' <- typing typsyn
                        corsyn <- C.typToCore typsyn'
                        return $ map (show . pretty) corsyn
                )
                =<< initSession
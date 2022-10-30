{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Plato.Test.Core.TermTransl where

import Plato.Common.Error
import Plato.Common.SrcLoc
import Plato.Core.Context
import Plato.Parsing.FixResol
import Plato.Parsing.Monad
import Plato.Parsing.Parser

import Plato.Syntax.Core
import qualified Plato.Syntax.Typing as T
import qualified Plato.Transl.PsToTyp as T
import qualified Plato.Transl.SrcToPs as P
import qualified Plato.Transl.TypToCore as C
import Plato.Typing.KindInfer
import Plato.Typing.TypeCheck

import Plato.Test.Core.Utils
import Plato.Test.Utils

import Control.Exception.Safe
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Plato.Typing.Renamer
import Test.Hspec

testcases :: [(String, IO (Term, Ty) -> Expectation)]
testcases =
        [
                ( "id : {a} a -> a; id = \\x -> x"
                , ( `shouldSatisfyReturn`
                        \case
                                ( TmRecord [(VN "id", TmTAbs (TVN "a") (TmAbs (VN "x") (TyVar 0 1) (TmVar 0 2)))]
                                        , TyRecord [(VN "id", TyAll (TVN "a") KnStar (TyArr (TyVar 0 1) (TyVar 0 1)))]
                                        ) -> True
                                _ -> False
                  )
                )
        ]

test :: (MonadThrow m, MonadIO m) => (String, m (Term, Ty) -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (ps, st) <- eitherToMonadThrow (P.parseLine (T.pack inp) declsParser)
                let opdict = opDict (parser_ust st)
                ps' <- mapM (resolve opdict) ps
                (fundecs, _) <- T.transDecls ps'
                fundecs' <- mapM (typeCheck M.empty) fundecs
                (fundec, _) <- renameFuncDs emptyRenameState fundecs'
                ((_, b), _) <- (StateT . C.transDecl) (noLoc $ T.ConD fundec) `runStateT` (emptyContext, emptyKnTable)
                case b of
                        TmAbbBind t tyT -> return (unLoc t, unLoc tyT)
                        _ -> unreachable ""
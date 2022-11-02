module Plato.Test.Core.PrettyTerm where

import Plato.Common.Error
import Plato.Common.SrcLoc

import Plato.Core.Context
import Plato.Core.Pretty

import Plato.Parsing.FixResol
import Plato.Parsing.Monad
import Plato.Parsing.Parser

import qualified Plato.Syntax.Typing as T

import qualified Plato.Transl.PsToTyp as T
import qualified Plato.Transl.SrcToPs as P
import qualified Plato.Transl.TypToCore as C

import Plato.Typing.KindInfer
import Plato.Typing.Renamer
import Plato.Typing.TypeCheck

import Control.Exception.Safe
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.String
import Test.Hspec

testcases :: [(String, IO String -> Expectation)]
testcases =
        [ ("id : {a} a -> a; id = \\x -> x", (`shouldReturn` "{ id = \\a. \\x:a. x } : { id : {a:*}. a -> a }"))
        ]

test :: (MonadThrow m, MonadIO m) => (String, m String -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (ps, st) <- eitherToMonadThrow (P.parseLine declsParser (T.pack inp) )
                let opdict = opDict (parser_ust st)
                ps' <- mapM (resolve opdict) ps
                (fundecs, _) <- T.transDecls ps'
                fundecs' <- mapM (typeCheck M.empty) fundecs
                (fundec, _) <- renameFuncDs emptyRenameState fundecs'
                ((_, b), _) <- (StateT . C.transDecl) (noLoc $ T.ConD fundec) `runStateT` (emptyContext, emptyKnTable)
                return $ renderString $ layoutPretty defaultLayoutOptions (ppr emptyContext b)
{-# LANGUAGE OverloadedStrings #-}

module Plato.Test.Typing.TopDeclPretty where

import Plato.Common.Error
import Plato.Common.SrcLoc
import Plato.Parsing.FixResol
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Syntax.Typing
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs

import Control.Exception.Safe
import Control.Monad.Writer
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.String
import Test.Hspec

testcases :: [(String, IO String -> Expectation)]
testcases =
        [ ("data Bool = True | False", (`shouldReturn` "[ Bool = μBool. <True | False>\n, True = fold [Bool] True : Bool\n, False = fold [Bool] False : Bool ]"))
        , ("data List a = Nil | a :: List a", (`shouldReturn` "[ List = \\a. μList. <Nil | :: a List a>\n, Nil = \\a. fold [List a] Nil : {a} List a\n, :: = \\a. \\2:a. \\3:List a. fold [List a] (:: 2 3) : {a} a -> List a -> List a ]"))
        ]

test :: MonadThrow m => (String, m String -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (ps, st) <- eitherToMonadThrow (parseLine (T.pack inp) topdeclParser)
                let opdict = opDict (parser_ust st)
                ps' <- resolve opdict ps
                (tydecs, fundecs, _) <- execWriterT $ transTopDecl ps'
                let decs = map unLoc tydecs ++ map (ConD . unLoc) fundecs
                return $ renderString $ layoutPretty defaultLayoutOptions (pretty decs)

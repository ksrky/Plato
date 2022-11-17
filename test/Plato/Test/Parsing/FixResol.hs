{-# LANGUAGE LambdaCase #-}

module Plato.Test.Parsing.FixResol where

import Plato.Syntax.Parsing
import qualified Plato.Syntax.Parsing as Parsing
import Plato.Transl.SrcToPs
import Plato.Types.Fixity
import Plato.Types.Location
import Plato.Types.Monad
import Plato.Types.Name
import Plato.Types.Name.Global

import Plato.Test.Parsing.Utils
import Plato.Test.Utils

import Control.Exception.Safe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

testcases :: [(String, IO (Expr GlbName) -> Expectation)]
testcases =
        [ ("x + y", (`shouldSatisfyReturn` \case OpE (VE "x") Plus (VE "y") -> True; _ -> False))
        , ("x + y * z", (`shouldSatisfyReturn` \case OpE (VE "x") Plus (L _ (OpE (VE "y") Times (VE "z"))) -> True; _ -> False))
        , ("x ++ y ++ z", (`shouldSatisfyReturn` \case OpE (L _ (OpE (VE "x") Append (VE "y"))) Append (VE "z") -> True; _ -> False))
        , ("x > y > z", (`shouldThrow` anyException))
        ]

fixityEnv :: FixityEnv GlbName
fixityEnv =
        M.fromList
                [ (GlbName Local (varName "+") NoSpan, Fixity 6 Leftfix)
                , (GlbName Local (varName "*") NoSpan, Fixity 7 Leftfix)
                , (GlbName Local (varName "++") NoSpan, Fixity 5 Leftfix)
                , (GlbName Local (varName ">") NoSpan, Fixity 4 Nonfix)
                ]

test :: (MonadThrow m, MonadFail m) => (String, m (Expr GlbName) -> Expectation) -> SpecWith ()
test (inp, iscorrect) =
        it inp $
                iscorrect $ do
                        Parsing.Program _ _ [L _ (Parsing.Eval exp)] <- returnPlato (exp2ps $ T.pack inp) initPInfo initPState{plt_fixityEnv = fixityEnv}
                        return $ unLoc exp
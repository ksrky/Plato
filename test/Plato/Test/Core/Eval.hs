module Plato.Test.Core.Eval where

import Plato.Common.Location
import Plato.Common.Monad
import Plato.Common.Name.Global
import Plato.Core.Context
import Plato.Core.Eval
import Plato.Syntax.Core
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Transl.TypToCore

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.RWS
import Data.Text.IO qualified as T
import Prettyprinter
import Prettyprinter.Render.String

import Plato.Transl.CoreToIO
import Test.Hspec

testcases :: [(String, IO String -> Expectation)]
testcases =
        [ ("test01.plt", (`shouldReturn` ""))
        , ("test02.plt", (`shouldReturn` ""))
        , ("test03.plt", (`shouldReturn` ""))
        , ("test04.plt", (`shouldReturn` ""))
        , ("test05.plt", (`shouldReturn` ""))
        , ("test06.plt", (`shouldReturn` ""))
        , ("test07.plt", (`shouldReturn` "True\n"))
        , ("test08.plt", (`shouldReturn` "Succ (Succ (Succ (Succ (Succ Zero))))\n"))
        , ("test09.plt", (`shouldReturn` "T1\n"))
        , ("test10.plt", (`shouldReturn` "T1 :: (T2 :: (T2 :: (T1 :: Nil)))\n"))
        , ("test11.plt", (`shouldReturn` "T1\n"))
        , ("test12.plt", (`shouldReturn` "Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))\n"))
        , ("test13.plt", (`shouldReturn` ""))
        , ("test14.plt", (`shouldReturn` "Succ (Succ (Succ Zero))\n"))
        , ("test15.plt", (`shouldReturn` "Succ Zero\n"))
        , ("test16.plt", (`shouldReturn` "Succ Zero :: (Succ (Succ Zero) :: (Succ (Succ (Succ Zero)) :: (Succ (Succ (Succ (Succ Zero))) :: Nil)))\n"))
        , ("test17.plt", (`shouldReturn` "Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))))))))))))))))\n"))
        -- , ("test18.plt", (`shouldReturn` "anything\n"))
        ]

test :: (MonadThrow m, MonadIO m) => (String, m String -> Expectation) -> SpecWith ()
test (fname, iscorrect) = it fname $
        iscorrect $ do
                let src = "test/testcases/" ++ fname
                inp <- liftIO $ T.readFile src
                ( returnPlato $ do
                                (fixenv, ps) <- src2ps inp
                                ps' <- psCanon [] fixenv ps
                                typ <- ps2typ ps'
                                mod <- typ2core typ
                                res <- processModule' mod
                                return $ renderString $ layoutPretty defaultLayoutOptions $ vsep res
                        )
                        initPInfo
                        initPState

processModule' :: MonadThrow m => Module -> Plato m [Doc ann]
processModule' mod = do
        ctx <- gets plt_glbContext
        let ctx' = foldl (flip $ uncurry addBinding) ctx (map (\(n, b) -> (toplevelName (moduleName mod) (noLoc n), b)) (moduleBind mod))
        return $ map (ppr ctx' . eval ctx') (moduleEval mod)
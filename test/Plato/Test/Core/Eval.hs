module Plato.Test.Core.Eval where

import Plato.Common.SrcLoc
import Plato.Core.Context
import Plato.Core.Eval
import Plato.Syntax.Core
import Plato.Transl.CoreToIO
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Transl.TypToCore

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Prettyprinter
import Prettyprinter.Render.String
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
        , -- , ("test13.plt", (`shouldReturn` ""))
          ("test14.plt", (`shouldReturn` "Succ (Succ (Succ Zero))\n"))
        , ("test15.plt", (`shouldReturn` "Succ Zero\n"))
        , ("test16.plt", (`shouldReturn` "Succ Zero :: (Succ (Succ Zero) :: (Succ (Succ (Succ Zero)) :: (Succ (Succ (Succ (Succ Zero))) :: Nil)))\n"))
        , ("test17.plt", (`shouldReturn` "Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))))))))))))))))\n"))
        , ("test18.plt", (`shouldReturn` "anything\n"))
        ]

test :: (MonadThrow m, MonadIO m) => (String, m String -> Expectation) -> SpecWith ()
test (fname, iscorrect) = it fname $
        iscorrect $ do
                let src = "test/testcases/" ++ fname
                inp <- liftIO $ T.readFile src
                (_, _, ps) <- src2ps M.empty inp
                typ <- fst <$> ps2typ M.empty ps
                cmds <- snd <$> typ2core [] emptyContext typ
                return $ renderString $ layoutPretty defaultLayoutOptions (pprcmds emptyContext cmds)

pprcmds :: Context -> [Command] -> Doc ann
pprcmds _ [] = emptyDoc
pprcmds ctx (Import{} : cmds) = pprcmds ctx cmds
pprcmds ctx ((Bind x bind) : cmds) = pprcmds (V.cons (x, bind) ctx) cmds
pprcmds ctx (Eval t : cmds) = ppr ctx (eval ctx (unLoc t)) <> pprcmds ctx cmds
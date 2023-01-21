module Plato.Transl.SrcToPs where

import Plato.Common.Error
import Plato.Common.Fixity
import Plato.Common.Monad
import Plato.Common.Name

import Plato.Parsing.Monad
import Plato.Parsing.Parser

import Plato.Common.Location
import Plato.Syntax.Parsing

import Control.Exception.Safe
import Control.Monad.RWS
import qualified Data.Map.Strict as M
import qualified Data.Text as T

src2ps :: MonadThrow m => T.Text -> Plato m (FixityEnv Name, Program)
src2ps inp = do
        file <- asks plt_fileName
        modn <- filePath2modName file
        ((imps, topds), st) <- eitherToMonadThrow (parse file inp parser)
        let new_fixenv = M.mapKeys (Qual (noLoc modn) . noLoc) $ ust_fixityEnv (parser_ust st) -- temp
        modify $ \ps -> ps{plt_fixityEnv = M.union (plt_fixityEnv ps) new_fixenv}
        return (ust_fixityEnv (parser_ust st), Program modn imps topds)
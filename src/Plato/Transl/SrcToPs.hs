module Plato.Trasnsl.SrcToPs where

import Plato.Types.Error
import Plato.Types.Fixity
import Plato.Types.Location
import Plato.Types.Monad
import Plato.Types.Name
import Plato.Types.Name.Global
import Plato.Types.Name.Reader

import Plato.Parsing.FixResol
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Parsing.Rename

import Plato.Syntax.Parsing

import Control.Exception.Safe (MonadThrow)
import Control.Monad.RWS
import Control.Monad.Reader
import qualified Data.Text as T

src2ps :: MonadThrow m => T.Text -> Plato m (FixityEnv LName, Program RdrName)
src2ps inp = do
        file <- asks plt_fileName
        (res, st) <- eitherToMonadThrow (parse file inp parser)
        return (ust_fixityEnv (parser_ust st), res)

canonical :: MonadThrow m => FixityEnv GlbName -> Program RdrName -> Plato m (Program GlbName)
canonical fixenv prg = do
        glbenv <- gets plt_glbNameEnv
        prg' <- runReaderT (rename prg) glbenv
        resolveFixity fixenv prg'

moduleName :: Program GlbName -> Maybe ModuleName
moduleName = (unLoc <$>) . Plato.Syntax.Parsing.ps_moduleDecl

importModule :: Program GlbName -> [Located ModuleName]
importModule = Plato.Syntax.Parsing.ps_importDecls
module Plato.Transl.SrcToPs where

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
import qualified Data.Map.Strict as M
import qualified Data.Text as T

src2ps :: MonadThrow m => T.Text -> Plato m (FixityEnv Name, Program RdrName)
src2ps inp = do
        file <- asks plt_fileName
        (res, st) <- eitherToMonadThrow (parse file inp parser)
        return (ust_fixityEnv (parser_ust st), res)

canonical :: MonadThrow m => FixityEnv Name -> Program RdrName -> Plato m (Program GlbName)
canonical fixenv prg = do
        glbenv <- gets plt_glbNameEnv
        (prg', glbenv') <- renameTopDecl prg glbenv
        fixenv' <- M.union (renameFixityEnv glbenv' fixenv) <$> gets plt_fixityEnv
        modify $ \s -> s{plt_fixityEnv = fixenv', plt_glbNameEnv = glbenv'}
        resolveFixity fixenv' prg'

moduleName :: Program GlbName -> Maybe ModuleName
moduleName = (unLoc <$>) . ps_moduleDecl

importModules :: Program RdrName -> [Located ModuleName]
importModules = ps_importDecls
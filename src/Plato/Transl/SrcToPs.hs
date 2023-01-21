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

import Control.Exception.Safe
import Control.Monad.RWS
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Text as T

src2ps :: MonadThrow m => T.Text -> Plato m (FixityEnv Name, Program RdrName)
src2ps inp = do
        file <- asks plt_fileName
        modn <- filePath2modName file
        ((imps, topds), st) <- eitherToMonadThrow (parse file inp parser)
        return (ust_fixityEnv (parser_ust st), Program modn imps topds)

psCanon :: MonadThrow m => [ModuleName] -> FixityEnv Name -> Program RdrName -> Plato m (Program GlbName)
psCanon deps fixenv prg = do
        glbenv <- pickGlbEnv deps <$> gets plt_glbEnv
        (prg', glbenv') <- renameProgram prg glbenv
        fixenv' <- M.union (renameFixityEnv glbenv' fixenv) <$> gets plt_fixityEnv
        modify $ \s -> s{plt_fixityEnv = fixenv', plt_glbEnv = plt_glbEnv s `M.union` glbenv'}
        resolveFixity fixenv' prg'

getModuleName :: Program GlbName -> ModuleName
getModuleName prg = case ps_moduleDecl prg of
        Just (L _ modn) -> modn
        Nothing -> unreachable "getModuleName"

importModules :: Program RdrName -> [Located ModuleName]
importModules = ps_importDecls
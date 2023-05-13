module Plato.Transl.SrcToPs where

import Plato.Common.Error
import Plato.Common.Fixity
import Plato.Common.Location
import Plato.Common.Monad
import Plato.Common.Name
import Plato.Common.Name.Global

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

src2ps :: MonadThrow m => T.Text -> Plato m (FixityEnv Name, Program Name)
src2ps inp = do
        file <- asks plt_fileName
        (res, st) <- eitherToMonadThrow (parse file inp parser)
        return (ust_fixityEnv (parser_ust st), res)

psCanon :: MonadThrow m => [ModuleName] -> FixityEnv Name -> Program Name -> Plato m (Program GlbName)
psCanon imp_modns fixenv prg = do
        glbenv <- filterGlbNameEnv imp_modns <$> gets plt_glbNameEnv
        (prg', glbenv') <- renameTopDecls prg glbenv
        fixenv' <- M.union (renameFixityEnv glbenv' fixenv) <$> gets plt_fixityEnv
        modify $ \s -> s{plt_fixityEnv = fixenv', plt_glbNameEnv = plt_glbNameEnv s `M.union` glbenv'}
        resolveFixity fixenv' prg'

exp2ps :: MonadThrow m => T.Text -> Plato m (Program GlbName)
exp2ps inp = do
        (expr, _) <- eitherToMonadThrow (parseLine inp exprParser)
        glbenv <- gets plt_glbNameEnv
        expr' <- runReaderT (rename `traverse` expr) (glbenv, 0)
        fixenv <- gets plt_fixityEnv
        topd <- Eval <$> runReaderT (resolve expr') fixenv
        return $ Program{ps_moduleDecl = Nothing, ps_importDecls = [], ps_topDecls = [noLoc topd]}

getModuleName :: Program GlbName -> ModuleName
getModuleName prg = case ps_moduleDecl prg of
        Just (L _ modn) -> modn
        Nothing -> unreachable "getModuleName"

importModules :: Program Name -> [Located ModuleName]
importModules = ps_importDecls
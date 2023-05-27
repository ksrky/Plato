{-# LANGUAGE LambdaCase #-}

module Plato.Nicifier (nicify) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Reader
import Data.List qualified

import Plato.Common.Location
import Plato.Nicifier.OpParser
import Plato.Nicifier.OpParser.Fixity
import Plato.Syntax.Parsing

nicify :: MonadThrow m => Program -> m Program
nicify tdecs = runReaderT (nicifyDecls tdecs) initFixityEnv

nicifyDecls :: (MonadReader env m, HasFixityEnv env, MonadThrow m) => [LDecl] -> m [LDecl]
nicifyDecls decs = do
        decss' <- forM decss $ \ds -> case ds of
                [] -> return []
                L _ DataD{} : _ -> return ds
                L _ FuncD{} : _ -> do
                        let sp = concatSpans (map getLoc ds)
                        fdss <- forM ds $ \case
                                L _ (FuncD fds) -> dropFixDecl <$> opParse fds
                                _ -> return []
                        return [L sp $ FuncD $ concat fdss]

        return $ concat decss'
    where
        decss :: [[LTopDecl]]
        decss =
                Data.List.groupBy
                        ( curry $ \case
                                (L _ FuncD{}, L _ FuncD{}) -> True
                                (L _ DataD{}, L _ DataD{}) -> True
                                _ -> False
                        )
                        decs

dropFixDecl :: [LFunDecl] -> [LFunDecl]
dropFixDecl = filter (\case (L _ FixDecl{}) -> False; _ -> True)
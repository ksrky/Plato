{-# LANGUAGE LambdaCase #-}

module Plato.Parsing.Nicifier where

import Data.List qualified

import Plato.Syntax.Parsing

nicifyDecls :: [Decl] -> [Decl]
nicifyDecls decs =
        concatMap
                ( \ds -> case ds of
                        [] -> []
                        (DataD{} : _) -> ds
                        (FuncD{} : _) -> [FuncD $ concatMap (\case (FuncD fds) -> fds; _ -> []) ds]
                )
                decs'
    where
        decs' :: [[Decl]]
        decs' =
                Data.List.groupBy
                        ( curry $ \case
                                (FuncD{}, FuncD{}) -> True
                                (DataD{}, DataD{}) -> True
                                _ -> False
                        )
                        decs
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Plato.Syntax.Parsing.Decl where

import qualified Data.List
import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Syntax.Parsing.Expr
import Plato.Syntax.Parsing.Pat
import Plato.Syntax.Parsing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LDecl = Located Decl

-- type LModule = Located Module

data Decl
        = -- FixityD Fixity (Located Name)

          -- | ModuleD Ident LModule
          DataD Ident [Ident] [(Ident, LType)]
        | FuncD LFunDecl
        deriving (Eq, Show)

-- newtype Module = Module [LDecl] deriving (Eq, Show)

-- | Ordering Decls

{-orderDecls :: [Decl] -> [Decl]
orderDecls =
        Data.List.sortOn $ \case
                -- FixityD{} -> (0 :: Int)
                -- ModuleD{} -> 1
                DataD{} -> 2
                FuncSigD{} -> 3
                FuncD{} -> 4-}

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
-- instance Substitutable Decl where
{-- substPath (OpenD path) = OpenD <$> substPath path
-- substPath dec@FixityD{} = return dec
-- substPath (ModuleD id mod) = ModuleD id <$> substPath `traverse` mod
substPath (DataD id params fields) =
        DataD id params
                <$> mapM (\(con, ty) -> (con,) <$> substPath `traverse` ty) fields
-- substPath (FuncSigD id ty) = FuncSigD id <$> substPath `traverse` ty
-- substPath (FuncD id params body) = FuncD id <$> mapM (substPath `traverse`) params <*> substPath `traverse` body-}

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Decl where
        pretty = undefined

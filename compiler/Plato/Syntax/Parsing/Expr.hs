module Plato.Syntax.Parsing.Expr where

import Plato.Common.Fixity
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Pretty
import Plato.Syntax.Parsing.Pat
import Plato.Syntax.Parsing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LExpr = Located Expr
type LLocDecl = Located LocDecl

type Clause = ([LPat], LExpr)

data Expr
    = VarE Ident
    | AppE LExpr LExpr
    | BinE LExpr Ident LExpr
    | LamE [LPat] LExpr
    | LetE [LLocDecl] LExpr
    | CaseE LExpr [(LPat, LExpr)]
    | FactorE LExpr
    deriving (Eq, Show)

data LocDecl
    = FunSpecD Ident LType
    | FunBindD Ident [Clause]
    | FixityD (Located Name) Fixity
    deriving (Eq, Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Expr where
    pretty' _ (VarE var) = pretty var
    pretty' p (AppE fun arg) = parenswPrec p 1 $ hsep [pretty' 1 fun, pretty' 2 arg]
    pretty' _ (BinE lhs op rhs) = parens $ hsep [pretty' 1 lhs, pretty op, pretty' 1 rhs]
    pretty' p (LamE pats body) = parenswPrec p 0 $ hsep [backslash <> hsep (map pretty pats), arrow, pretty body]
    pretty' _ (LetE decs body) =
            hsep ["let", braces $ map pretty decs `sepBy` semi, "in", pretty body]
    pretty' p (CaseE match alts) =
            parenswPrec p 0
                    $ hsep [ "case" , pretty match, "of"
                           , braces $ map (\(p, e) -> hsep [pretty p, arrow, pretty e]) alts `sepBy` semi]
    pretty' _ (FactorE exp) = parens $ pretty exp

prClause :: Clause -> Doc ann
prClause (pats, exp) = hsep (map (pretty' 1) pats ++ [equals, pretty exp])

instance Pretty LocDecl where
    pretty (FunSpecD id ty) = hsep [pretty id, colon, pretty ty]
    pretty (FunBindD id clauses) = vsep [hsep [pretty id, prClause clause] | clause <- clauses]
    pretty (FixityD name (Fixity prec dir)) = hsep [pretty dir, pretty prec, pretty name]

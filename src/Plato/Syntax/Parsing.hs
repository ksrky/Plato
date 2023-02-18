module Plato.Syntax.Parsing where

import Plato.Common.Fixity
import Plato.Common.Location
import Plato.Common.Name

import Prettyprinter

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
type LName = Located Name
type LPsName = Located PsName
type LExpr = Located Expr
type LPat = Located Pat
type LType = Located Type
type LDecl = Located Decl
type LTopDecl = Located TopDecl

data PsName
        = Unqual LName
        | Qual (Located ModuleName) LName
        deriving (Ord, Show)

instance Eq PsName where
        Unqual (L _ n1) == Unqual (L _ n2) = n1 == n2
        Qual (L _ modn1) (L _ n1) == Qual (L _ modn2) (L _ n2) = modn1 == modn2 && n1 == n2
        _ == _ = False

data Expr
        = VarE LPsName
        | AppE LExpr LExpr
        | OpE LExpr LPsName LExpr
        | LamE [LName] LExpr
        | LetE [LDecl] LExpr
        | CaseE LExpr [(LPat, LExpr)]
        | FactorE LExpr -- removed after fixity resolution
        deriving (Eq, Show)

data Pat
        = ConP LPsName [LPat]
        | VarP LName
        | WildP
        deriving (Eq, Show)

data Type
        = VarT LName
        | ConT LPsName
        | AppT LType LType
        | ArrT LType LType
        | AllT [LName] LType
        deriving (Eq, Show)

data Decl
        = FuncD LName [LName] LExpr
        | FuncTyD LName LType
        | FixityD FixDir FixPrec [LName]
        deriving (Eq, Show)

data TopDecl
        = DataD LName [LName] [(LName, LType)]
        | Decl LDecl
        | Eval LExpr
        deriving (Eq, Show)

data Program = Program
        { ps_moduleName :: ModuleName
        , ps_imports :: [Located ModuleName]
        , ps_topDecls :: [LTopDecl]
        }
        deriving (Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
hsep' :: [Doc ann] -> Doc ann
hsep' docs = if null docs then emptyDoc else emptyDoc <+> hsep docs

instance Pretty PsName where
        pretty (Unqual n) = pretty n
        pretty (Qual modn n) = hcat [pretty modn, dot, pretty n]

instance Pretty Expr where
        pretty (VarE var) = pretty var
        pretty exp@AppE{} = pprapp exp
        pretty (OpE lhs op rhs) = pretty lhs <+> pretty op <+> pretty rhs
        pretty (LamE vars body) = backslash <> hsep (map pretty vars) <+> "->" <+> pretty body
        pretty (LetE decs body) =
                "let" <+> lbrace <> line
                        <> indent 4 (vsep (map pretty decs))
                        <> line
                        <> rbrace <+> "in" <+> pretty body
        pretty (CaseE match alts) =
                "case" <+> pretty match <+> "of" <+> lbrace <> line
                        <> indent 4 (vsep (map (\(pat, body) -> pretty pat <+> "->" <+> pretty body) alts))
                        <> line
                        <> rbrace
        pretty (FactorE exp) = pretty exp

pprapp :: Expr -> Doc ann
pprapp e = walk e []
    where
        walk :: Expr -> [Expr] -> Doc ann
        walk (AppE e1 e2) es = walk (unLoc e1) (unLoc e2 : es)
        walk e' es = pprParendExpr e' <+> sep (map pprParendExpr es)
        pprParendExpr :: Expr -> Doc ann
        pprParendExpr e@VarE{} = pretty e
        pprParendExpr e = parens (pretty e)

instance Pretty Pat where
        pretty (ConP con pats) = pretty con <> hsep' (map pprpat pats)
        pretty (VarP var) = pretty var
        pretty WildP = "_"

pprpat :: LPat -> Doc ann
pprpat pat@(L _ (ConP con pats))
        | null pats = pretty con
        | otherwise = parens $ pretty pat
pprpat pat = pretty pat

instance Pretty Type where
        pretty (VarT var) = pretty var
        pretty (ConT con) = pretty con
        pretty (AppT fun arg) = pretty fun <+> pprty AppPrec arg
        pretty (ArrT arg res) = pprty ArrPrec arg <+> "->" <+> pprty TopPrec res
        pretty (AllT vars body) = lbrace <> hsep (map pretty vars) <> rbrace <+> pretty body

data Prec = TopPrec | ArrPrec | AppPrec | AtomPrec deriving (Enum)

precty :: Type -> Prec
precty AllT{} = TopPrec
precty ArrT{} = ArrPrec
precty _ = AtomPrec

pprty :: Prec -> LType -> Doc ann
pprty p (L _ ty)
        | fromEnum p >= fromEnum (precty ty) = parens (pretty ty)
        | otherwise = pretty ty

instance Pretty Decl where
        pretty (FuncD var args body) = pretty var <> hsep' (map pretty args) <+> equals <+> pretty body
        pretty (FuncTyD var body_ty) = pretty var <+> colon <+> pretty body_ty
        pretty (FixityD dir prec ops) = hsep [pretty dir, pretty prec, concatWith (surround comma) (map pretty ops)]

instance Pretty TopDecl where
        pretty (DataD con args fields) =
                pretty con <> hsep' (map pretty args) <+> equals
                        <+> concatWith (\d e -> d <+> pipe <+> e) (map (\(c, ty) -> pretty c <> colon <+> pretty ty) fields)
        pretty (Decl dec) = pretty dec
        pretty (Eval exp) = pretty exp

instance Pretty Program where
        pretty (Program mod imps topdecs) =
                pretty mod <> line
                        <> vsep (map pretty imps)
                        <> line -- temp
                        <> vsep (map pretty topdecs)
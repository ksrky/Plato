{-# LANGUAGE OverloadedStrings #-}

module Plato.Syntax.Parsing where

import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Types.Fixity

import Prettyprinter

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
type LName = Located Name
type LExpr a = Located (Expr a)
type LPat a = Located (Pat a)
type LType a = Located (Type a)
type LDecl a = Located (Decl a)
type LTopDecl a = Located (TopDecl a)

type LArg = LName

data Expr a
        = VarE (Located a)
        | AppE (LExpr a) (LExpr a)
        | OpE (LExpr a) (Located a) (LExpr a)
        | LamE [LArg] (LExpr a)
        | LetE [LDecl a] (LExpr a)
        | CaseE (LExpr a) [(LPat a, LExpr a)]
        | FactorE (LExpr a) -- removed after fixity resolution
        deriving (Eq, Show)

data Pat a
        = ConP (Located a) [LPat a]
        | VarP LArg
        | WildP
        deriving (Eq, Show)

data Type a
        = VarT LArg
        | ConT (Located a)
        | AppT (LType a) (LType a)
        | ArrT (LType a) (LType a)
        | AllT [LArg] (LType a)
        deriving (Eq, Show)

data Decl a
        = FuncD LName [LArg] (LExpr a)
        | FuncTyD LName (LType a)
        | FixityD FixDir FixPrec LName
        deriving (Eq, Show)

data TopDecl a
        = DataD LName [LArg] [(LName, [LType a])]
        | TypeD LName [LArg] (LType a)
        | Decl (LDecl a)
        | Eval (LExpr a)
        deriving (Eq, Show)

data Program a = Program
        { moduleDecl :: Maybe (Located ModuleName)
        , importDecls :: [Located ModuleName]
        , topDecls :: [LTopDecl a]
        }
        deriving (Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
hsep' :: [Doc ann] -> Doc ann
hsep' docs = if null docs then emptyDoc else emptyDoc <+> hsep docs

instance Pretty a => Pretty (Expr a) where
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

pprapp :: Pretty a => Expr a -> Doc ann
pprapp e = walk e []
    where
        walk :: Pretty a => Expr a -> [Expr a] -> Doc ann
        walk (AppE e1 e2) es = walk (unLoc e1) (unLoc e2 : es)
        walk e' es = pprParendExpr e' <+> sep (map pprParendExpr es)
        pprParendExpr :: Pretty a => Expr a -> Doc ann
        pprParendExpr e@VarE{} = pretty e
        pprParendExpr e = parens (pretty e)

instance Pretty a => Pretty (Pat a) where
        pretty (ConP con pats) = pretty con <> hsep' (map pprpat pats)
        pretty (VarP var) = pretty var
        pretty WildP = "_"

pprpat :: Pretty a => LPat a -> Doc ann
pprpat pat@(L _ (ConP con pats))
        | null pats = pretty con
        | otherwise = parens $ pretty pat
pprpat pat = pretty pat

instance Pretty a => Pretty (Type a) where
        pretty (VarT var) = pretty var
        pretty (ConT con) = pretty con
        pretty (AppT fun arg) = pretty fun <+> pprty AppPrec arg
        pretty (ArrT arg res) = pprty ArrPrec arg <+> "->" <+> pprty TopPrec res
        pretty (AllT vars body) = lbrace <> hsep (map pretty vars) <> rbrace <+> pretty body

data Prec = TopPrec | ArrPrec | AppPrec | AtomPrec deriving (Enum)

precty :: Type a -> Prec
precty AllT{} = TopPrec
precty ArrT{} = ArrPrec
precty _ = AtomPrec

pprty :: Pretty a => Prec -> LType a -> Doc ann
pprty p (L _ ty)
        | fromEnum p >= fromEnum (precty ty) = parens (pretty ty)
        | otherwise = pretty ty

instance Pretty a => Pretty (Decl a) where
        pretty (FuncD var args body) = pretty var <> hsep' (map pretty args) <+> equals <+> pretty body
        pretty (FuncTyD var body_ty) = pretty var <+> colon <+> pretty body_ty
        pretty (FixityD dir prec op) = hsep [pretty dir, pretty prec, pretty op]

instance Pretty a => Pretty (TopDecl a) where
        pretty (DataD con args fields) =
                pretty con <> hsep' (map pretty args) <+> equals
                        <+> concatWith (\d e -> d <+> pipe <+> e) (map (\(c, tys) -> pretty c <> hsep' (map pretty tys)) fields)
        pretty (TypeD con args body) = pretty con <> hsep' (map pretty args) <+> equals <+> pretty body
        pretty (Decl dec) = pretty dec
        pretty (Eval exp) = pretty exp

instance Pretty a => Pretty (Program a) where
        pretty (Program mod imps topdecs) =
                maybe emptyDoc (\d -> pretty d <> line) mod
                        <> vsep (map (\imp -> "import" <+> pretty imp) imps)
                        <> vsep (map pretty topdecs)
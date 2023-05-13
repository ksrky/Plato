module Plato.Syntax.Typing where

import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Name.Global

import Data.IORef
import qualified Data.Map.Strict as M
import Prettyprinter

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
type LName = Located Name
type LExpr = Located Expr
type LPat = Located Pat
type LType = Located Type
type LDecl = Located Decl
type LArg = LName

-- | Expressions
data Expr
        = VarE GlbName
        | AppE Expr Expr
        | AbsE LArg (Maybe Type) Expr
        | TAppE Expr [Type]
        | TAbsE [LArg] Expr
        | LetE [FuncD] Expr
        | ProjE Expr GlbName
        | RecordE [(GlbName, Expr)]
        | CaseE Expr (Maybe Type) [(Pat, Expr)]
        | TagE GlbName [Expr] Type
        | FoldE Type
        | AnnE Expr Sigma
        deriving (Eq, Show)

-- | Patterns
data Pat
        = ConP GlbName [Pat]
        | VarP LArg
        | WildP
        deriving (Eq, Show)

-- | Types
data Type
        = VarT TyVar
        | ConT GlbName
        | ArrT Type Type
        | AllT [(TyVar, Maybe Kind)] Rho
        | AbsT LArg (Maybe Kind) Type
        | AppT Type Type
        | RecT LArg Kind Type
        | RecordT [(GlbName, Type)]
        | SumT [(LName, [Type])]
        | MetaT MetaTv
        deriving (Eq, Show)

type Sigma = Type
type Rho = Type
type Tau = Type

data TyVar
        = BoundTv LName
        | SkolemTv LName Uniq
        deriving (Show)

data MetaTv = Meta Uniq TyRef

type TyRef = IORef (Maybe Tau)

type Uniq = Int

-- | Kinds
data Kind
        = MetaK MetaKv
        | StarK
        | ArrK Kind Kind
        deriving (Eq, Show)

data MetaKv = MetaKv Uniq KnRef

type KnRef = IORef (Maybe Kind)

-- | Function decl
data FuncD = FuncD LName Expr Type deriving (Eq, Show)

data Decl
        = TypeD LName Type
        | VarD LName Type
        | ConD FuncD
        deriving (Eq, Show)

data Program = Program
        { typ_modn :: ModuleName
        , typ_decls :: [LDecl]
        , typ_binds :: [FuncD]
        , typ_body :: [(LExpr, Type)]
        }
        deriving (Eq, Show)

type TyEnv = M.Map GlbName Sigma
type KnEnv = M.Map GlbName Kind

----------------------------------------------------------------
-- Set Eq and Show class
----------------------------------------------------------------
instance Eq TyVar where
        (BoundTv s1) == (BoundTv s2) = unLoc s1 == unLoc s2
        (SkolemTv _ u1) == (SkolemTv _ u2) = u1 == u2
        _ == _ = False

instance Eq MetaTv where
        (Meta u1 _) == (Meta u2 _) = u1 == u2

instance Show MetaTv where
        show (Meta u _) = "$" ++ show u

instance Eq MetaKv where
        (MetaKv u1 _) == (MetaKv u2 _) = u1 == u2

instance Show MetaKv where
        show (MetaKv u _) = "$" ++ show u

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
hsep' :: [Doc ann] -> Doc ann
hsep' docs = if null docs then emptyDoc else emptyDoc <+> hsep docs

instance Pretty Expr where
        pretty (VarE var) = pretty var
        pretty (AppE (FoldE ty) exp) = "fold" <+> lbracket <> pretty ty <> rbracket <+> pprexpr exp
        pretty exp@AppE{} = pprapp exp
        pretty (AbsE var mty body) = backslash <> pretty var <> maybe emptyDoc ((colon <>) . pretty) mty <> dot <+> pretty body
        pretty (TAppE fun tyargs) = pretty fun <> hsep' (map pretty tyargs)
        pretty (TAbsE vars body) = backslash <> hsep (map pretty vars) <> dot <+> pretty body
        pretty (LetE decs body) = hsep ["let", lbrace <> line, indent 4 (vsep (map pretty decs)), line <> rbrace, "in", pretty body]
        pretty (ProjE exp lab) = surround "." (pretty exp) (pretty lab)
        pretty (RecordE fields) =
                hsep
                        [ lbrace
                        , concatWith (\d -> (<+> comma <+> d)) (map (\(var, exp) -> pretty var <+> equals <+> pretty exp) fields)
                        , rbrace
                        ]
        pretty (CaseE match mty alts) =
                "case"
                        <+> sep [pretty match, colon, maybe emptyDoc pretty mty]
                        <+> "of"
                        <+> lbrace
                                <> line
                                <> indent 4 (vsep (map (\(pat, body) -> pretty pat <+> "->" <+> pretty body) alts))
                                <> line
                                <> rbrace
        pretty (TagE con args _) = pretty con <> hsep' (map pprexpr args)
        pretty (FoldE ty) = sep [lbracket, pretty ty, rbracket]
        pretty (AnnE exp ty) = pprexpr exp <+> colon <+> pretty ty

pprexpr :: Expr -> Doc ann
pprexpr e@VarE{} = pretty e
pprexpr e@(TagE _ as _) | null as = pretty e
pprexpr e = parens (pretty e)

pprapp :: Expr -> Doc ann
pprapp e = walk e []
    where
        walk :: Expr -> [Expr] -> Doc ann
        walk (AppE e1 e2) es = walk e1 (e2 : es)
        walk e' es = pprexpr e' <+> sep (map pprexpr es)

instance Pretty Pat where
        pretty (ConP con pats) = pretty con <+> hsep (map pprpat pats)
        pretty (VarP var) = pretty var
        pretty WildP = "_"

pprpat :: Pat -> Doc ann
pprpat pat@(ConP con pats)
        | null pats = pretty con
        | otherwise = parens $ pretty pat
pprpat pat = pretty pat

instance Pretty TyVar where
        pretty (BoundTv n) = pretty n
        pretty (SkolemTv n _) = pretty n

instance Pretty Type where
        pretty (VarT var) = pretty var
        pretty (ConT con) = pretty con
        pretty (AppT fun arg) = pretty fun <+> pprty AppPrec arg
        pretty (ArrT arg res) = pprty ArrPrec arg <+> "->" <+> pprty TopPrec res
        pretty (AllT vars body) = lbrace <> hsep (map (pretty . fst) vars) <> rbrace <+> pretty body
        pretty (AbsT var mkn body) = sep [backslash <> pretty var <> maybe emptyDoc ((colon <>) . pretty) mkn] <> dot <+> pretty body
        pretty (RecT var _ body) = "μ" <> pretty var <> dot <+> pretty body
        pretty (RecordT fields) =
                hsep
                        [ lbrace
                        , concatWith (\d -> (<+> comma <+> d)) (map (\(var, exp) -> pretty var <+> colon <+> pretty exp) fields)
                        , rbrace
                        ]
        pretty (SumT fields) =
                langle
                        <> concatWith
                                (\d e -> d <+> pipe <+> e)
                                ( map
                                        (\(c, tys) -> pretty c <> hsep' (map (pprty AppPrec) tys))
                                        fields
                                )
                        <> rangle
        pretty (MetaT tv) = viaShow tv

data Prec = TopPrec | ArrPrec | AppPrec | AtomPrec deriving (Enum)

precty :: Type -> Prec
precty AllT{} = TopPrec
precty ArrT{} = ArrPrec
precty _ = AtomPrec

pprty :: Prec -> Type -> Doc ann
pprty p ty
        | fromEnum p >= fromEnum (precty ty) = parens (pretty ty)
        | otherwise = pretty ty

instance Pretty Kind where
        pretty StarK = "*"
        pretty (ArrK k1 k2) = pprkind k1 <+> pretty k2
        pretty (MetaK m) = viaShow m

pprkind :: Kind -> Doc ann
pprkind StarK = pretty StarK
pprkind kn = parens (pretty kn)

instance Pretty FuncD where
        pretty (FuncD var body body_ty) = hsep [pretty var, equals, pretty body, colon, pretty body_ty]

instance Pretty Decl where
        pretty (TypeD con body) = hsep [pretty con, equals, pretty body]
        pretty (VarD var ty) = hsep [pretty var, colon, pretty ty]
        pretty (ConD fund) = pretty fund

instance Pretty Program where
        pretty (Program mod decs binds body) =
                pretty mod
                        <> line
                        <> vsep (map pretty decs)
                        <> (if null decs then emptyDoc else line)
                        <> vsep (map pretty binds)
                        <> (if null binds then emptyDoc else line)
                        <> vsep (map (pretty . fst) body)
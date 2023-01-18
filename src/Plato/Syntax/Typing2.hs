{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}

module Plato.Syntax.Typing2 where

import Plato.Types.Location
import Plato.Types.Name

import Data.IORef
import qualified Data.Map.Strict as M
import Prettyprinter

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
type family NoLoc a where
        NoLoc Name = Name
        NoLoc Arg = Arg
        NoLoc (Expr f) = Expr f
        NoLoc (Pat f) = Pat f
        NoLoc (Type f) = Type f
        NoLoc (TyVar f) = TyVar f
        NoLoc (FuncD f) = FuncD f

type Arg = Name

-- | Expressions
data Expr (f :: * -> *)
        = VarE (f Name)
        | AppE (f (Expr f)) (f (Expr f))
        | AbsE (f Arg) (Maybe (f (Type f))) (f (Expr f))
        | TAppE (f (Expr f)) [f (Type f)]
        | TAbsE [(TyVar f, Maybe Kind)] (f (Expr f))
        | LetE [FuncD f] (f (Expr f))
        | ProjE (f (Expr f)) (f Name)
        | RecordE [(f Name, f (Expr f))]
        | CaseE (f (Expr f)) (Maybe (f (Type f))) [(f (Pat f), f (Expr f))]
        | TagE Name [f (Expr f)] (Type f)
        | FoldE (Type f)
        | RefE Name (f Name)
        deriving (Eq, Show)

-- | Patterns
data Pat (f :: * -> *)
        = ConP (f Name) [f (Pat f)]
        | VarP (f Arg)
        | WildP
        deriving (Eq, Show)

-- | Types
data Type (f :: * -> *)
        = VarT (TyVar f)
        | ConT (f Name)
        | ArrT (f (Type f)) (f (Type f))
        | AllT [(TyVar f, Maybe Kind)] (f (Rho f))
        | AppT (f (Type f)) (f (Type f))
        | AbsT (f Arg) (Maybe Kind) (f (Type f))
        | RecT (f Arg) Kind (f (Type f))
        | RecordT [(f Name, f (Type f))]
        | SumT [(f Name, [f (Type f)])]
        | MetaT MetaTv
        deriving (Eq, Show)

type Sigma f = Type f
type Rho f = Type f
type Tau f = Type f

data TyVar (f :: * -> *)
        = BoundTv (f Name)
        | SkolemTv (f Name) Uniq
        deriving (Show, Ord)

data MetaTv = MetaTv Uniq (IORef (Maybe (Tau NoLoc)))

type Uniq = Int

tyVarName :: TyVar -> Located Name
tyVarName (BoundTv x) = x
tyVarName (SkolemTv x _) = x

-- | Kinds
data Kind
        = StarK
        | ArrK Kind Kind
        | MetaK MetaKv
        deriving (Eq, Show)

data MetaKv = MetaKv Uniq (IORef (Maybe Kind))

-- | Function decl
data FuncD f = FuncD (Located Name) (f (Expr f)) (Type f) deriving (Eq, Show)

data Decl
        = TypeD (Located Name) (Type NoLoc)
        | VarD (Located Name) (Type NoLoc)
        | ConD (FuncD NoLoc)
        deriving (Eq, Show)

data Module = Module
        { typ_modn :: ModuleName
        , typ_decls :: [Located Decl]
        , typ_binds :: [FuncD]
        , typ_body :: [(Located Expr, Located Type)]
        }
        deriving (Eq, Show)

type TyEnv = M.Map Name Sigma
type KnEnv = M.Map Name Kind

----------------------------------------------------------------
-- Set Eq and Show class
----------------------------------------------------------------
instance Eq (TyVar f) where
        (BoundTv s1) == (BoundTv s2) = unLoc s1 == unLoc s2
        (SkolemTv _ u1) == (SkolemTv _ u2) = u1 == u2
        _ == _ = False

instance Eq MetaTv where
        (MetaTv u1 _) == (MetaTv u2 _) = u1 == u2

instance Show MetaTv where
        show (MetaTv u _) = "$" ++ show u

instance Ord MetaTv where
        MetaTv u1 _ `compare` MetaTv u2 _ = u1 `compare` u2

instance Eq MetaKv where
        (MetaKv u1 _) == (MetaKv u2 _) = u1 == u2

instance Show MetaKv where
        show (MetaKv u _) = "$" ++ show u

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
hsep' :: [Doc ann] -> Doc ann
hsep' docs = if null docs then emptyDoc else emptyDoc <+> hsep docs

instance Pretty (Expr f) where
        pretty (VarE var) = pretty var
        pretty (AppE (L _ (FoldE ty)) exp) = "fold" <+> lbracket <> pretty ty <> rbracket <+> pprexpr (unLoc exp)
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
                "case" <+> sep [pretty match, colon, maybe emptyDoc pretty mty] <+> "of" <+> lbrace <> line
                        <> indent 4 (vsep (map (\(pat, body) -> pretty pat <+> "->" <+> pretty body) alts))
                        <> line
                        <> rbrace
        pretty (TagE con args _) = pretty con <> hsep' (map (pprexpr . unLoc) args)
        pretty (FoldE ty) = sep [lbracket, pretty ty, rbracket]
        pretty (RefE modn var) = hcat [pretty modn, dot, pretty var]

pprexpr :: Expr -> Doc ann
pprexpr e@VarE{} = pretty e
pprexpr e@(TagE _ as _) | null as = pretty e
pprexpr e = parens (pretty e)

pprapp :: Expr -> Doc ann
pprapp e = walk e []
    where
        walk :: Expr -> [Expr] -> Doc ann
        walk (AppE e1 e2) es = walk (unLoc e1) (unLoc e2 : es)
        walk e' es = pprexpr e' <+> sep (map pprexpr es)

instance Pretty (Pat f) where
        pretty (ConP con pats) = pretty con <+> hsep (map (pprpat . unLoc) pats)
        pretty (VarP var) = pretty var
        pretty WildP = "_"

pprpat :: Pat -> Doc ann
pprpat pat@(ConP con pats)
        | null pats = pretty con
        | otherwise = parens $ pretty pat
pprpat pat = pretty pat

instance Pretty (TyVar f) where
        pretty (BoundTv n) = pretty n
        pretty (SkolemTv n _) = pretty n

instance Pretty Type where
        pretty (VarT var) = pretty var
        pretty (ConT con) = pretty con
        pretty (AppT fun arg) = pretty fun <+> pprty AppPrec (unLoc arg)
        pretty (ArrT arg res) = pprty ArrPrec (unLoc arg) <+> "->" <+> pprty TopPrec (unLoc res)
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
                                        (\(c, tys) -> pretty c <> hsep' (map (pprty AppPrec . unLoc) tys))
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

instance Pretty (FuncD f) where
        pretty (FuncD var body body_ty) = hsep [pretty var, equals, pretty body, colon, pretty body_ty]

instance Pretty Decl where
        pretty (TypeD con body) = hsep [pretty con, equals, pretty body]
        pretty (VarD var ty) = hsep [pretty var, colon, pretty ty]
        pretty (ConD fund) = pretty fund

instance Pretty Module where
        pretty (Module mod decs binds body) =
                pretty mod <> line
                        <> vsep (map pretty decs)
                        <> (if null decs then emptyDoc else line)
                        <> vsep (map pretty binds)
                        <> (if null binds then emptyDoc else line)
                        <> vsep (map (pretty . fst) body)
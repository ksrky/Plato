module Plato.Syntax.Typing where

import Plato.Common.Location
import Plato.Common.Name

import Data.IORef
import qualified Data.Map.Strict as M
import Prettyprinter

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
type LName = Located Name
type LTypName = Located TypName
type LExpr = Located Expr
type LPat = Located Pat
type LType = Located Type
type LDecl = Located Decl

data TypName = TypName [LName] LName deriving (Eq, Show)

-- | Expressions
data Expr
        = VarE LTypName
        | AppE LExpr LExpr
        | AbsE LName (Maybe Type) LExpr
        | PAbsE LPat (Maybe Type) LExpr
        | TAppE LExpr [Type]
        | TAbsE [(TyVar, Maybe Kind)] LExpr
        | LetE Binds LExpr
        | CaseE LExpr (Maybe Type) [([LPat], LExpr)]
        | PBarE LExpr LExpr
        deriving (Eq, Show)

-- | Patterns
data Pat
        = ConP LTypName [LPat]
        | VarP LName
        | WildP
        deriving (Eq, Show)

-- | Types
data Type
        = VarT TyVar
        | ConT LTypName
        | ArrT LType LType
        | AllT [(TyVar, Maybe Kind)] (Located Rho)
        | AppT LType LType
        | AbsT LName (Maybe Kind) LType
        | RecT LName (Maybe Kind) LType
        | RecordT [(LName, LType)]
        | SumT [(LName, [LType])]
        | MetaT MetaTv
        deriving (Eq, Show)

type Sigma = Type
type Rho = Type
type Tau = Type

data TyVar
        = BoundTv LName
        | SkolemTv LName Uniq
        deriving (Show, Ord)

data MetaTv = MetaTv Uniq (IORef (Maybe Tau))

type Uniq = Int

tyVarName :: TyVar -> LName
tyVarName (BoundTv x) = x
tyVarName (SkolemTv x _) = x

-- | Kinds
data Kind
        = StarK
        | ArrK Kind Kind
        | MetaK MetaKv
        deriving (Eq, Show)

data MetaKv = MetaKv Uniq (IORef (Maybe Kind))

data Mod
        = ModName LName
        | ModPath Mod LName
        deriving (Eq, Show)

-- | Function decl
data Binds = Binds [(LName, LExpr)] [(LName, Type)] deriving (Eq, Show)

data Decl
        = TypeD LName Type Kind
        | ConD LName Type
        deriving (Eq, Show)

data Module = Module
        { typ_modn :: ModuleName
        , typ_decls :: [Decl]
        , typ_binds :: Binds
        , typ_body :: [(LExpr, Type)]
        }
        deriving (Eq, Show)

type TyEnv = M.Map Name Sigma
type KnEnv = M.Map Name Kind
type ModEnv = M.Map Name TypEnv
data TypEnv = TypEnv TyEnv KnEnv ModEnv

----------------------------------------------------------------
-- class
----------------------------------------------------------------
instance Eq TyVar where
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

instance Ord MetaKv where
        MetaKv u1 _ `compare` MetaKv u2 _ = u1 `compare` u2

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
hsep' :: [Doc ann] -> Doc ann
hsep' docs = if null docs then emptyDoc else emptyDoc <+> hsep docs

instance Pretty TypName where
        pretty (TypName quals name) = hcat [foldr (surround dot) (dot <> pretty name) (map pretty quals)]

instance Pretty Expr where
        pretty (VarE var) = pretty var
        pretty exp@AppE{} = pprapp exp
        pretty (AbsE var mty body) = backslash <> pretty var <> maybe emptyDoc ((colon <>) . pretty) mty <> dot <+> pretty body
        pretty (PAbsE pat mty body) = backslash <> pretty pat <> maybe emptyDoc ((colon <>) . pretty) mty <> dot <+> pretty body
        pretty (TAppE fun tyargs) = pretty fun <> hsep' (map pretty tyargs)
        pretty (TAbsE vars body) = backslash <> hsep (map pretty vars) <> dot <+> pretty body
        pretty (LetE binds body) = hsep ["let", lbrace <> line, indent 4 (pretty binds), line <> rbrace, "in", pretty body]
        pretty (CaseE match mty alts) =
                "case" <+> sep [pretty match, colon, maybe emptyDoc pretty mty] <+> "of" <+> lbrace <> line
                        <> indent 4 (vsep (map (\(pat, body) -> pretty pat <+> "->" <+> pretty body) alts))
                        <> line
                        <> rbrace
        pretty (PBarE lhs rhs) = hsep [pretty lhs, pipe, pretty rhs]

pprexpr :: Expr -> Doc ann
pprexpr e@VarE{} = pretty e
pprexpr e = parens (pretty e)

pprapp :: Expr -> Doc ann
pprapp e = walk e []
    where
        walk :: Expr -> [Expr] -> Doc ann
        walk (AppE e1 e2) es = walk (unLoc e1) (unLoc e2 : es)
        walk e' es = pprexpr e' <+> sep (map pprexpr es)

instance Pretty Pat where
        pretty (ConP con pats) = pretty con <+> hsep (map (pprpat . unLoc) pats) -- temp
        pretty (VarP var) = pretty var
        pretty WildP = "_"

pprpat :: Pat -> Doc ann
pprpat pat@(ConP con pats) -- temp
        | null pats = pretty con
        | otherwise = parens $ pretty pat
pprpat pat = pretty pat

instance Pretty TyVar where
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

instance Pretty Binds where
        pretty (Binds _binds _sigs) = undefined {-temp-}

instance Pretty Decl where
        pretty (TypeD con body kn) = hsep [pretty con, equals, pretty body, dot, pretty kn]
        pretty (ConD var sig) = hsep [pretty var, colon, pretty sig]

instance Pretty Module where
        pretty (Module mod decs binds body) =
                pretty mod <> line
                        <> vsep (map pretty decs)
                        <> (if null decs then emptyDoc else line)
                        <> pretty binds
                        <> line
                        <> vsep (map (pretty . fst) body)
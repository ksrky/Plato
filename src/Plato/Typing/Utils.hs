module Plato.Typing.Utils where

import Data.List
import Plato.Common.Error
import Plato.Common.Info
import Plato.Common.Pretty
import Plato.Typing.Syntax

instance Pretty Expr where
        pretty (VarExpr _ x) = show x
        pretty (AppExpr _ e1 e2) = "(" ++ pretty e1 ++ " " ++ pretty e2 ++ ")"
        pretty (TAppExpr _ e1 ty2) = "(" ++ pretty e1 ++ " [" ++ pretty ty2 ++ "])"
        pretty (LamExpr _ x e) = "(\\" ++ pretty x ++ "->" ++ pretty e ++ ")"
        pretty (LetExpr _ d e) = "(let {" ++ pretty d ++ "} in " ++ pretty e ++ ")"
        pretty (ProjExpr _ e l) = pretty e ++ "." ++ pretty l
        pretty (RecordExpr _ fields) =
                let pf i (li, ei) = (if show li /= show i then pretty li ++ "=" else "") ++ pretty ei
                    pfs i l = case l of
                        [] -> ""
                        [f] -> pf i f
                        f : rest -> pf i f ++ ", " ++ pfs (i + 1) rest
                 in "{" ++ pfs 1 fields ++ "}"
        pretty (TagExpr _ l es) =
                let pa t =
                        let pptm = pretty t
                         in if ' ' `elem` pptm then "(" ++ pptm ++ ")" else pptm
                 in if null es then pretty l else "(" ++ pretty l ++ " " ++ unwords (map pa es) ++ ")"
        pretty (CaseExpr _ e alts) =
                let pa (pi, ei) = pretty pi ++ "->" ++ pretty ei
                 in "(case " ++ pretty e ++ " of {" ++ intercalate " | " (map pa alts) ++ "})"

instance Pretty Pat where
        pretty (ConPat _ l pats) = if null pats then pretty l else "(" ++ pretty l ++ " " ++ unwords (map pretty pats) ++ ")"
        pretty (AnyPat _ (Just x)) = pretty x
        pretty (AnyPat _ Nothing) = "_"

instance Pretty Type where
        pretty (VarType _ x) = pretty x
        pretty (ArrType _ ty1 ty2) = "(" ++ pretty ty1 ++ " -> " ++ pretty ty2 ++ ")"
        pretty (AllType _ x ty) = "(∀" ++ pretty x ++ ". " ++ pretty ty ++ ")"
        pretty (AbsType _ x ty) = "(λ" ++ pretty x ++ ". " ++ pretty ty ++ ")"
        pretty (AppType _ ty1 ty2) = "(" ++ pretty ty1 ++ " " ++ pretty ty2 ++ ")"
        pretty (RecType _ x ty) = "(μ" ++ pretty x ++ ". " ++ pretty ty ++ ")"
        pretty (RecordType _ fields) =
                let pf i (li, ei) = (if show li /= show i then pretty li ++ "=" else "") ++ pretty ei
                    pfs i l = case l of
                        [] -> ""
                        [f] -> pf i f
                        f : rest -> pf i f ++ ", " ++ pfs (i + 1) rest
                 in "{" ++ pfs 1 fields ++ "}"
        pretty (SumType fields) =
                let pf (_, li, tys) = pretty li ++ if null tys then "" else " " ++ unwords (map pretty tys)
                 in intercalate " | " (map pf fields)

instance Pretty Decl where
        pretty (TypeDecl _ x ty) = pretty x ++ " = " ++ pretty ty
        pretty (VarDecl _ x ty) = pretty x ++ " : " ++ pretty ty
        pretty (FuncDecl _ x e ty) = pretty x ++ " = " ++ pretty e ++ " : " ++ pretty ty

instance Pretty Decls where
        pretty (Decls modns decls (body, bodyty)) =
                "import {\n\t"
                        ++ intercalate "\n\t" (map pretty modns)
                        ++ "\n}\nbinds {\n\t"
                        ++ intercalate "\n\t" (map pretty decls)
                        ++ "\n}\nmain="
                        ++ pretty body
                        ++ " : "
                        ++ pretty bodyty

instance GetInfo Expr where
        getInfo (VarExpr fi _) = fi
        getInfo (AppExpr fi _ _) = fi
        getInfo (TAppExpr fi _ _) = fi
        getInfo (LamExpr fi _ _) = fi
        getInfo (LetExpr fi _ _) = fi
        getInfo (ProjExpr fi _ _) = fi
        getInfo (RecordExpr fi _) = fi
        getInfo (CaseExpr fi _ _) = fi
        getInfo (TagExpr fi _ _) = fi

instance GetInfo Type where
        getInfo (VarType fi _) = fi
        getInfo (ArrType fi _ _) = fi
        getInfo (AllType fi _ _) = fi
        getInfo (AbsType fi _ _) = fi
        getInfo (AppType fi _ _) = fi
        getInfo (RecType fi _ _) = fi
        getInfo (RecordType fi _) = unreachable "RecordType does not have Info"
        getInfo (SumType _) = unreachable "SumType does not have Info"

instance GetInfo Decl where
        getInfo (TypeDecl fi _ _) = fi
        getInfo (VarDecl fi _ _) = fi
        getInfo (FuncDecl fi _ _ _) = fi

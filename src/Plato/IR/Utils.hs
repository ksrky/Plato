module Plato.IR.Utils where

import Plato.Common.Error
import Plato.Common.Info
import Plato.Common.Pretty
import Plato.IR.Syntax

instance Pretty Expr where
        pretty (VarExpr _ x) = pretty x
        pretty (AppExpr _ e1 e2) = "(" ++ pretty e1 ++ " " ++ pretty e2 ++ ")"
        pretty (TAppExpr _ e1 ty2) = "(" ++ pretty e1 ++ " [" ++ pretty ty2 ++ "])"
        pretty (LamExpr _ x e) = "(\\" ++ pretty x ++ " " ++ pretty e ++ ")"
        pretty (LetExpr _ d e) = "(let {" ++ pretty d ++ "} in " ++ pretty e ++ ")"
        pretty (ProjExpr _ e l) = pretty e ++ "." ++ pretty l
        pretty (RecordExpr _ fields) =
                let pf i (li, ei) = (if show li /= show i then pretty li ++ "=" else "") ++ pretty ei
                    pfs i l = case l of
                        [] -> ""
                        [f] -> pf i f
                        f : rest -> pf i f ++ ", " ++ pfs (i + 1) rest
                 in "{" ++ pfs 1 fields ++ "}"
        pretty _ = undefined

instance Pretty Type where
        pretty _ = ""

instance Pretty Decl where
        pretty _ = ""

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
        getInfo (RecordType fi _) = unreachable "RecordType does not have Info"
        getInfo (SumType _) = unreachable "SumType does not have Info"
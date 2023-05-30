module Plato.Core.Codegen where

import Plato.Syntax.Core
import Prettyprinter

class Codegen a where
        codegen :: a -> Doc ann

instance Codegen Term where
        codegen (TmVar i _) = pretty i
        codegen (TmApp t1 t2) = parens $ codegen t1 <> parens (codegen t2)
        codegen (TmAbs _ tyT1 t2) = parens $ hcat [pipe, codegen tyT1, dot, codegen t2]
        codegen (TmTApp t1 tyT2) = parens $ codegen t1 <> parens (codegen tyT2)
        codegen (TmTAbs _ knK1 t2) = parens $ hcat [pipe, codegen knK1, codegen t2]
        codegen TmLet{} = undefined
        codegen (TmFix t) = parens $ "fix" <> parens (codegen t)
        codegen (TmProj t i) = hcat [codegen t, dot, pretty i]
        codegen (TmRecord fields) =
                hcat [lbrace, concatWith (surround comma) (map (codegen . snd) fields), rbrace]
        codegen (TmInj i tyT elims) =
                hcat
                        [ "inj_"
                        , pretty i
                        , brackets $ pretty tyT
                        , parens $ concatWith (surround comma) (map codegen elims)
                        ]
        codegen (TmCase t alts) =
                hcat ["case_", codegen t, hcat (map (\(_, t) -> pipe <> codegen t) alts)]
        codegen (TmFold tyT) = parens $ hcat ["fold", brackets $ pretty tyT]
        codegen (TmUnfold tyT) = parens $ hcat ["unfold", brackets $ pretty tyT]

instance Codegen Type where
        codegen (TyVar i _) = pretty i
        codegen (TyFun tyT1 tyT2) = parens $ hcat [codegen tyT1, "->", codegen tyT2]
        codegen (TyAll _ knK1 tyT2) = parens $ hcat [squote, codegen knK1, dot, codegen tyT2]
        codegen (TyApp tyT1 tyT2) = parens $ codegen tyT1 <> parens (codegen tyT2)
        codegen (TyAbs _ knK1 tyT2) = parens $ hcat [pipe, codegen knK1, dot, codegen tyT2]
        codegen (TyRec _ knK1 tyT2) = parens $ hcat [slash, codegen knK1, dot, codegen tyT2]
        codegen (TyRecord fields) = braces $ concatWith (surround comma) (map (codegen . snd) fields)
        codegen (TySum fields) = parens $ concatWith (surround pipe) (map codegen fields)

instance Codegen Kind where
        codegen KnStar = "*"
        codegen (KnFun knK1 knK2) = parens $ hcat [codegen knK1, "->", codegen knK2]

instance Codegen Binding where
        codegen NameBind = emptyDoc
        codegen (TmVarBind tyT) = colon <> codegen tyT
        codegen (TyVarBind knK) = colon <> codegen knK
        codegen (TmAbbBind t _) = equals <> codegen t
        codegen (TyAbbBind tyT _) = equals <> codegen tyT

instance Codegen Command where
        codegen (Bind _ bind) = codegen bind
        codegen (Eval t) = codegen t
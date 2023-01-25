module Plato.Transl.CoreToIO where

{-
import Plato.Core.Context
import Plato.Core.Debug
import Plato.Core.Eval

import Plato.Syntax.Core

import Plato.Common.Error
import Plato.Common.Monad
import Plato.Common.Name

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RWS
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.Text

processModule :: (MonadThrow m, MonadIO m) => Module -> Plato m ()
processModule mod = do
        ctx <- gets plt_context
        let ctx' = foldl (flip $ uncurry addBinding) ctx (moduleBind mod)
        opt <- asks plt_isEntry
        when opt $ mapM_ (printResult ctx') (moduleEval mod)
        modify $ \s -> s{plt_context = ctx'}

printResult :: MonadIO m => Context -> Term -> m ()
printResult ctx t = liftIO $ putDoc $ ppr ctx $ eval ctx t

-- putDoc $ vsep [viaShow ctx, P.ppr ctx $ eval ctx t]

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
hsep' :: [Doc ann] -> Doc ann
hsep' docs = if null docs then emptyDoc else emptyDoc <+> hsep docs

ppr :: Context -> Term -> Doc ann
ppr ctx t = pprtm t <> line
    where
        pprtm :: Term -> Doc ann
        pprtm t =
                case t of
                        TmVar _ x -> pretty $ index2name ctx x
                        TmApp (TmFold _) t2 | isTag t2 -> pprtm t2
                        TmTApp t1 _ -> pprtm t1
                        TmTag fi op [t1, t2] _ | isConOp (actualName fi) -> pprtm2 t1 <+> pretty op <+> pprtm2 t2
                        TmTag _ li ts _ -> hcat [pretty li, hsep' (map pprtm1 ts)]
                        _ -> unreachable "Something unexpected occured."

        isTag :: Term -> Bool
        isTag TmTag{} = True
        isTag _ = False

        isConOp :: Name -> Bool
        isConOp = (':' ==) . T.head . nameText

        pprtm1 :: Term -> Doc ann
        pprtm1 t@TmVar{} = pprtm t
        pprtm1 (TmApp (TmFold _) t2) | isTag t2 = pprtm1 t2
        pprtm1 t@(TmTag _ _ as _) | null as = pprtm t
        pprtm1 t = parens $ pprtm t

        pprtm2 :: Term -> Doc ann
        pprtm2 t@TmVar{} = pprtm t
        pprtm2 (TmApp (TmFold _) t2) | isTag t2 = pprtm2 t2
        pprtm2 t@(TmTag fi _ [_, _] _) | isConOp (actualName fi) = parens $ pprtm t
        pprtm2 t@TmTag{} = pprtm t
        pprtm2 t = parens $ pprtm t-}
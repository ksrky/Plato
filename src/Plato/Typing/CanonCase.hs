module Plato.Typing.CanonCase where

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Name
import Plato.Syntax.Typing

arity :: Name -> Int
arity = undefined

constructors :: Name -> [LName]
constructors = undefined

subst :: LExpr -> LExpr -> LName -> LExpr
subst = undefined

isVar :: ([LPat], LExpr) -> Bool
isVar (L _ WildP{} : _, _) = True
isVar (L _ VarP{} : _, _) = True
isVar (L _ ConP{} : _, _) = False
isVar ([], _) = unreachable ""

isCon :: ([LPat], LExpr) -> Bool
isCon = not . isVar

getCon :: ([LPat], LExpr) -> Name
getCon (L _ (ConP c _) : _, _) = unLoc c
getCon _ = undefined

makeVar :: Int -> String
makeVar k = "_u" ++ show k

partition :: (([LPat], LExpr) -> Bool) -> [([LPat], LExpr)] -> [[([LPat], LExpr)]]
partition _ [] = []
partition _ [x] = [[x]]
partition f (x : x' : xs)
        | f x == f x' = tack x (partition f (x' : xs))
        | otherwise = [x] : partition f (x' : xs)

tack :: a -> [[a]] -> [[a]]
tack x xss = (x : head xss) : tail xss

match :: Int -> [LExpr] -> [([LPat], LExpr)] -> LExpr -> LExpr
match _ [] qs def = foldr (\e1 e2 -> noLoc $ PBarE e1 e2) def [e | ([], e) <- qs]
match k us qs def = foldr (matchVarCon k us) def (partition isVar qs)

matchVarCon :: Int -> [LExpr] -> [([LPat], LExpr)] -> LExpr -> LExpr
matchVarCon k us qs def
        | isVar (head qs) = matchVar k us qs def
        | otherwise = matchCon k us qs def

matchVar :: Int -> [LExpr] -> [([LPat], LExpr)] -> LExpr -> LExpr
matchVar k (u : us) qs def = match k us [(ps, subst e u v) | (L _ (VarP v) : ps, e) <- qs] def
matchVar _ _ _ _ = error "unreachable"

matchCon :: Int -> [LExpr] -> [([LPat], LExpr)] -> LExpr -> LExpr
matchCon k (u : us) qs def = noLoc $ CaseE u Nothing [matchClause c k us (choose c qs) def | c <- cs]
    where
        cs = constructors (getCon (head qs))
matchCon _ _ _ _ = error "unreachable"

matchClause :: LName -> Int -> [LExpr] -> [([LPat], LExpr)] -> LExpr -> ([LPat], LExpr)
matchClause c k us qs def =
        ( [noLoc $ ConP c (map (noLoc . VarP) us')]
        , match (k + k') (map (noLoc . VarE) us' ++ us) [(ps ++ ps', e) | (L _ (ConP _ ps') : ps, e) <- qs] def
        )
    where
        k' = arity (unLoc c)
        us' = [noLoc $ str2varName $ makeVar (i + k) | i <- [1 .. k']]

choose :: LName -> [([LPat], LExpr)] -> [([LPat], LExpr)]
choose c qs = [q | q <- qs, getCon q == unLoc c]

canonCase :: LExpr -> [(LPat, LExpr)] -> LExpr
canonCase test alts = match 0 [test] (map (\(p, e) -> ([p], e)) alts) undefined

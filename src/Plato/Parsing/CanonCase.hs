module Plato.Parsing.CanonCase where

import Plato.Common.Name
import Plato.Syntax.Parsing

arity :: Name -> Int
arity = undefined

constructors :: Name -> [Name]
constructors = undefined

data Exp
        = CASE String [Clause]
        | FATBAR Exp Exp

data Clause = CLAUSE String [String] Exp

subst :: Exp -> String -> String -> Exp
subst = undefined

type Equation = ([Pat], Exp)

isVar :: Equation -> Bool
isVar (VAR{} : _, _) = True
isVar (CON{} : _, _) = False

isCon :: Equation -> Bool
isCon = not . isVar

getCon :: Equation -> String
getCon (CON c _ : _, _) = c

makeVar :: Int -> String
makeVar k = "_u" ++ show k

partition :: Eq b => (a -> b) -> [a] -> [[a]]
partition _ [] = []
partition _ [x] = [[x]]
partition f (x : x' : xs)
        | f x == f x' = tack x (partition f (x' : xs))
        | otherwise = [x] : partition f (x' : xs)

tack :: a -> [[a]] -> [[a]]
tack x xss = (x : head xss) : tail xss

match :: Int -> [String] -> [Equation] -> Exp -> Exp
match _ [] qs def = foldr FATBAR def [e | ([], e) <- qs]
match k (u : us) qs def = foldr (matchVarCon k (u : us)) def (partition isVar qs)

matchVarCon :: Int -> [String] -> [Equation] -> Exp -> Exp
matchVarCon k us qs def
        | isVar (head qs) = matchVar k us qs def
        | otherwise = matchCon k us qs def

matchVar :: Int -> [String] -> [([Pat], Exp)] -> Exp -> Exp
matchVar k (u : us) qs def = match k us [(ps, subst e u v) | (VAR v : ps, e) <- qs] def
matchVar _ _ _ _ = error "unreachable"

matchCon :: Int -> [String] -> [Equation] -> Exp -> Exp
matchCon k (u : us) qs def = CASE u [matchClause c k us (choose c qs) def | c <- cs]
    where
        cs = constructors (getCon (head qs))
matchCon _ _ _ _ = error "unreachable"

matchClause :: String -> Int -> [String] -> [([Pat], Exp)] -> Exp -> Clause
matchClause c k us qs def = CLAUSE c us' (match (k + k') (us' ++ us) [(ps ++ ps', e) | (CON _ ps' : ps, e) <- qs] def)
    where
        k' = arity c
        us' = [makeVar (i + k) | i <- [1 .. k']]

choose :: String -> [Equation] -> [Equation]
choose c qs = [q | q <- qs, getCon q == c]
module Logic.NK.Propositional

import Plato.Base;

data Proof p = QED;

axiom : forall p. Proof p;
axiom = QED;


data FALSE;

data And p q;

data Or p q;

data Not p;

data Imply p q;


-- | Introduction rules
introAnd : forall p q. Proof p -> Proof q -> Proof (And p q);
introAnd _ _ = axiom;

introOrL : forall p q. Proof p -> Proof (Or p q);
introOrL _ = axiom;

introOrR : forall p q. Proof q -> Proof (Or p q);
introOrR _ = axiom;

introImpl : forall p q. (Proof p -> Proof q) -> Proof (Imply p q);
introImpl _ = axiom;

introNot : forall p. (Proof p -> Proof FALSE) -> Proof (Not p);
introNot _ = axiom;

contradicts : forall p. Proof p -> Proof (Not p) -> Proof FALSE;
contradicts _ _ = axiom;

contradicts' : forall p. Proof (Not p) -> Proof p -> Proof FALSE;
contradicts' = flip contradicts;

-- | Elimination Rules
elimAndL : forall p q. Proof (And p q) -> Proof p;
elimAndL _ = axiom;

elimAndR : forall p q. Proof (And p q) -> Proof q;
elimAndR _ = axiom;

elimOr : forall p q r. (Proof p -> Proof r) -> (Proof q -> Proof r) -> Proof (Or p q) -> Proof r;
elimOr _ _ _ = axiom;

elimImpl : forall p q. Proof p -> Proof (Imply p q) -> Proof q;
elimImpl _ _ = axiom;

modusPonens : forall p q. Proof (Imply p q) -> Proof p -> Proof q;
modusPonens = flip elimImpl;

absurd : forall p. Proof FALSE -> Proof p;
absurd _ = axiom;

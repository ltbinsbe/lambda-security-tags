
module Types.Labelled where

import Types.Shared

type Label    = Int    --labels

data Program  = Program [Decl] Term

data Term     -- computations
              = TVar Var Label
              | TApp Term Term Label
              | TITE Term Term Term Label
              | TLet Var Label Term Term Label
              | TAs Term Tag Label 
              | TDrop Term Tag Label
              -- values / terminals
              | TLam {- \ -} Var Label {- : -} Type {- . -} Term Label
              | TBool Bool Label
              | TInt Int Label
              | TRef Ref Label {- no concrete syntax -}
              | TArray [Term] Label {- array shorthand as concrete syntax? -}

-- | `labelOf t` is `Just l` if `t` is a term labelled with `l`
-- | or is `Nothing` if `t` is not labelled
labelOf :: Term -> Label
labelOf t = case t of 
  TVar _ l  -> l
  TApp _ _ l -> l
  TITE _ _ _ l -> l
  TLet _ _ _ _ l -> l
  TAs _ _ l -> l
  TDrop _ _ l -> l
  TLam _ _ _ _ l -> l
  TBool _ l -> l
  TInt _ l -> l
  TRef _ l -> l
  TArray _ l -> l

-- | If `t` is a term labelled with `l` then `repLabel t l'` is the (same) term
-- labelled with `l'`. If `t` is not labelled then `repLabel t l'` equals `t`
repLabel :: Label -> Term -> Term
repLabel l t = case t of 
  TVar v _ -> TVar v l
  TApp x y _ -> TApp x y l
  TITE x y z _ -> TITE x y z l
  TLet a b c d _ -> TLet a b c d l
  TAs x y _ -> TAs x y l
  TDrop x y _ -> TDrop x y l
  TLam a b c d _ -> TLam a b c d l  
  TBool b _ -> TBool b l
  TInt i _ -> TInt i l
  TRef r _ -> TRef r l
  TArray es _ -> TArray es l

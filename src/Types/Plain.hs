
module Types.Plain where

import Types.Shared

data Program  = Program [Decl] Term (Maybe Type)

data Term     -- computations
              = TVar Var
              | TApp Term Term
              | TITE Term Term Term
              | TLet Var Term Term
              | TAs Term Tag
              | TDrop Term Tag
              -- values / terminals
              | TLam {- \ -} Var {- : -} Type {- . -} Term Tag
              | TBool Bool Tag
              | TInt Int Tag
              | TRef Ref Tag {- no concrete syntax -}
              | TArray [Term] Tag {- array shorthand as concrete syntax? -}

valTagOf :: Term {- value -} -> Tag
valTagOf t = case t of 
  TLam _ _ _ tag  -> tag
  TBool _ tag     -> tag
  TInt _ tag      -> tag
  TRef _ tag      -> tag
  TArray _ tag    -> tag
  _               -> error "valTagOf"

valRepTag :: Tag -> Term {- value -} -> Term
valRepTag tag t = case t of 
  TLam x y z _    -> TLam x y z tag
  TBool b _       -> TBool b tag
  TInt i _        -> TInt i tag
  TRef r _        -> TRef r tag
  TArray es _     -> TArray es tag
  t               -> t


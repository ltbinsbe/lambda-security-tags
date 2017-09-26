
module Types.Plain where

import Types.Shared

data Program  = Program [Decl] Term

data Term     -- computations
              = TVar Var
              | TApp Term Term
              | TITE Term Term Term
              | TLet Var Term Term
              | TAs Term Tag
              | TDrop Term Tag
              -- values / terminals
              | TLam {- \ -} Var {- : -} Type {- . -} Term
              | TBool Bool
              | TInt Int
              | TRef Ref {- no concrete syntax -}
              | TArray [Term] {- array shorthand as concrete syntax? -}



module Types.Labelled where

import Types.Shared

type Label    = Int    --labels

data Program  = Program [Decl] Term

data Term     -- computations
              = TVar Var Label
              | TApp Term Term Label
              | TITE Term Term Term Label
              | TLet Var Label Term Term Label
              | TAs Term Tag
              | TDrop Term Tag
              -- values / terminals
              | TLam {- \ -} Var Label {- : -} Type {- . -} Term Label
              | TBool Bool Label
              | TInt Int Label
              | TRef Ref Label {- no concrete syntax -}
              | TArray [Term] Label {- array shorthand as concrete syntax? -}


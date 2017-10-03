
module Semantics.Builtins where

import Types.Shared
import Types.Plain
import Types.EDSL

int_type, bool_type :: Type
int_type  = TyInt Top
bool_type = TyBool Top

plus_type :: Type
plus_type = TyArrow int_type (TyArrow int_type int_type Top) Top

plus_term :: Term 
plus_term = TLam "#X" (TyInt Top) 
              (TLam "#Y" (TyInt Top) (TPlus (TVar "#X") (TVar "#Y")) Top) Top

or_type :: Type
or_type = bool_type --> bool_type --> bool_type

or_term :: Term
or_term = TLam "#X" (TyBool Top) 
              (TLam "#Y" (TyBool Top) (TOr (TVar "#X") (TVar "#Y")) Top) Top


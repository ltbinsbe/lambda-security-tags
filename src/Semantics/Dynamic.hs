
module Semantics.Dynamic where

import Types.Shared
import Types.Plain

subs :: Var -> Term {- value -} -> Term -> Term
subs x v t = case t of 
  TVar y | x == y     -> v
         | otherwise  -> TVar y
  TApp t1 t2          -> TApp (subs x v t1) (subs x v t2)
  TITE t1 t2 t3       -> TITE (subs x v t1) (subs x v t2) (subs x v t3)
  TLet y t1 t2        -> TLet y (subs x v t1) $ case x == y of
                          True  -> t2 -- if let binds `x` then `x` is not free in `t2`
                          False -> subs x v t2
  TAs t1 tag          -> TAs (subs x v t1) tag
  TDrop t1 tag        -> TAs (subs x v t1) tag
  TLam y ty t1 tag 
    | x == y          -> TLam y ty t1 tag -- `x` not free in body `t1`
    | otherwise       -> TLam y ty (subs x v t1) tag
  TBool b tag         -> TBool b tag
  TInt i tag          -> TInt i tag
  TRef r tag          -> TRef r tag
  TArray es tag       -> TArray (map (subs x v) es) tag
    

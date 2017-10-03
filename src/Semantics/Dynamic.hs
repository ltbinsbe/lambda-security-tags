
module Semantics.Dynamic (step, steps, eval) where

import Types.Shared
import Types.Plain
import Semantics.Operators
import Semantics.Builtins

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
  TPlus t1 t2         -> TPlus (subs x v t1) (subs x v t2)
  TOr t1 t2           -> TOr (subs x v t1) (subs x v t2)

isVal :: Term -> Bool
isVal t = case t of
  TBool _ _     -> True
  TInt _ _      -> True
  TRef _ _      -> True
  TArray _ _    -> True
  TLam _ _ _ _  -> True
  _             -> False

eval :: Program -> Term
eval (Program ds term _) = steps (gHierarchy ds) term

steps :: AnnHier -> Term -> Term {- value -}
steps hier t  | isVal t   = t
              | otherwise = steps hier (step hier t)

-- TODO, refactor such that rules have separate functions
step :: AnnHier -> Term -> Term
step hier t = case t of 
  TVar "plus" -> plus_term
  TVar "or"   -> or_term 
  TVar x -> error ("unbound variable: " ++ x)
  TApp t1 t2 | isVal t1 && isVal t2 -> case t1 of 
    TLam x ty t tag -> subs x t2 t
    _               -> error ("not an abstraction in application")
  --simplification: left-to-right evaluation
  TApp t1 t2 | isVal t1 -> TApp t1 (premise t2)
  TApp t1 t2            -> TApp (premise t1) t2
  TITE t1 t2 t3 | isVal t1-> case t1 of
    TBool True s1         -> t2
    TBool False s1        -> t3
    _                     -> error ("guard of if-then-else not a boolean")
  TITE t1 t2 t3 -> TITE (premise t1) t2 t3
  TLet x t1 t2 | isVal t1 -> subs x t1 t2 
  TLet x t1 t2            -> TLet x (premise t1) t2
  TAs t1 tag | isVal t1   -> valRepTag (tg_prod (valTagOf t1) tag) t1 
  TAs t1 tag              -> TAs (premise t1) tag
  TDrop t1 tag | isVal t1 -> valRepTag (gCutOp hier (valTagOf t1) tag) t1
  TDrop t1 tag            -> TDrop (premise t1) tag
  TLam _ _ _ _            -> error "step on value"
  TBool _ _               -> error "step on value"
  TInt _ _                -> error "step on value"
  TRef _ _                -> error "step on value"
  TArray _ _              -> error "step on value"
  TPlus t1 t2             -> case (t1, t2) of 
        (TInt x Top, TInt y Top) -> TInt (x + y) Top
        _                        -> error ("builtin: plus")
  TOr t1 t2               -> case (t1, t2) of 
        (TBool x Top, TBool y Top)  -> TBool (x || y) Top
        _                           -> error ("builtin: or")
  where
    premise = step hier



module Semantics.Static where

import Types.Shared
import Types.Plain
import Semantics.Operators

import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)

type Env      = M.Map Var Type

typecheck :: Program -> Type -> Bool
typecheck (Program ds t) = isOfType (gHierarchy ds) t
  
typings :: Program -> [Type]
typings (Program ds t) = typesOf (gHierarchy ds) t

isOfType :: AnnHier -> Term -> Type -> Bool
isOfType hier term ty2 = any compare (typesOf hier term)
  where compare ty1 = ty1 `typeEq` ty2 && gSubTagOp hier (tagOf ty1) (tagOf ty2)

typesOf :: AnnHier -> Term -> [Type]
typesOf hier term = apply_rules hier M.empty term 

type Rule = AnnHier -> Env -> Term -> [Type]

apply_rules :: Rule 
apply_rules hier env term = 
      rule_var hier env term 
  ++  rule_bool hier env term
  ++  rule_abs hier env term
  ++  rule_app hier env term
  ++  rule_let hier env term
  ++  rule_if hier env term
  ++  rule_as hier env term
  ++  rule_drop hier env term 

rule_var :: Rule
rule_var hier env term = case term of 
  TVar v  | Just ty <- M.lookup v env -> [ty]
  _                                   -> []

rule_bool :: Rule
rule_bool hier env term = case term of
  TBool _ tag -> [TyBool tag]
  _           -> []

rule_int :: Rule
rule_int hier env term = case term of
  TInt _ tag  -> [TyInt tag]
  _           -> []

rule_abs :: Rule
rule_abs hier env term = case term of 
  TLam x ty t tag -> do
    ty2 <- apply_rules hier (M.insert x ty env) t
    return $ TyArrow ty ty2 tag 
  _           -> []

rule_app :: Rule
rule_app hier env term = case term of
  TApp t1 t2  -> do 
    ty_arr <- apply_rules hier env t1 
    case ty_arr of  
      TyArrow ty1 ty2 s3  -> do ty3 <- apply_rules hier env t2
                                guard (ty1 `typeEq` ty3)
                                guard (gSubTagOp hier (tagOf ty3) (tagOf ty1))
                                return ty2
      _                 -> []
  _           -> []

rule_let :: Rule
rule_let hier env term = case term of
  TLet x t1 t2 -> do 
    ty1 <- apply_rules hier env t1 
    let s1 = tagOf ty1 
    apply_rules hier (M.insert x ty1 env) t2
  _            -> []

rule_if :: Rule
rule_if hier env term = case term of
  TITE t1 t2 t3 -> do
    ty1 <- apply_rules hier env t1
    guard (isBool ty1)
    ty2 <- apply_rules hier env t2
    let s2 = tagOf ty2
    ty3 <- apply_rules hier env t3
    let s3 = tagOf ty3
    guard (ty2 `typeEq` ty3)
    return (replaceTag (gIntersectionOp hier s2 s3) ty2) 
  _             -> []

rule_as :: Rule
rule_as hier env term = case term of  
  TAs t s2  -> do 
    ty <- apply_rules hier env t
    let s1 = tagOf ty
    return (replaceTag (tg_prod s1 s2) ty)
  _         -> []

rule_drop :: Rule
rule_drop hier env term = case term of
  TDrop t s'  -> do
    ty <- apply_rules hier env t
    let s = tagOf ty
    return (replaceTag (gCutOp hier s s') ty)
  _           -> []



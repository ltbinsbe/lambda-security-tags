
module Semantics.Static where

import Types.Shared
import Types.Plain

import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)

-- `a1` -> `a2` means `a2` inherits from `a1` (i.e. `a2` is more specific)
type AnnHier  = M.Map Ann [Ann] 
type Env      = M.Map Var Type

typecheck :: Program -> Type -> Bool
typecheck (Program ds t) = isOfType (gHierarchy ds) t
  
typings :: Program -> [Type]
typings (Program ds t) = typesOf (gHierarchy ds) t

isOfType :: AnnHier -> Term -> Type -> Bool
isOfType hier term tyB = any compare (typesOf hier term)
  where compare tyS = tyS `typeEq` tyB && gSubTagOp hier (tagOf tyS) (tagOf tyB)

typesOf :: AnnHier -> Term -> [Type]
typesOf hier term = apply_rules hier M.empty term 

gHierarchy :: [Decl] -> AnnHier
gHierarchy ds = foldr op M.empty ds
         where op (SecAnn a) = M.insertWith (++) a []
               op (SecAnnExt a2 a1) = M.insertWith (++) a1 [a2]

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
  TBool _ -> [TyBool Top]
  _       -> []

rule_abs :: Rule
rule_abs hier env term = case term of 
  TLam x ty t -> do
    ty2 <- apply_rules hier (M.insert x ty env) t
    return $ TyArrow ty ty2 Top
  _           -> []

rule_app :: Rule
rule_app hier env term = case term of
  TApp t1 t2  -> do 
    ty_arr <- apply_rules hier env t1 
    case ty_arr of  
      TyArrow ty1 ty2 s3  -> do ty3 <- apply_rules hier env t2
                                guard (ty1 `typeEq` ty3)
                                guard (gSubTagOp hier (tagOf ty1) (tagOf ty3))
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
    return (replaceTag (TgProd s1 s2) ty)
  _         -> []

rule_drop :: Rule
rule_drop hier env term = case term of
  TDrop t s'  -> do
    ty <- apply_rules hier env t
    let s = tagOf ty
    return (replaceTag (gCutOp hier s s') ty)
  _           -> []

type TypeOp a = Tag -> Tag -> a -- answers `a1` inherits from `a2`?

-- | Generates `<:` based on a hierarchy obtained from annotation declarations
gSubTagOp :: AnnHier -> TypeOp Bool
gSubTagOp hier p q = case (p,q) of
  (Bot, _)                  -> True   -- implies Bot <: Bot
  (_, Bot)                  -> False
  (_, Top)                  -> True   -- implies Top <: Top
  (Top, _)                  -> False
  -- Find in the hierarchy whether `a2` is `a1` or any of the descendents of `a1`
  (TgAnn a1, TgAnn a2)      -> isDescendant hier a1 a2
  (TgAnn _, TgProd _ _)     -> flatten p `extends` flatten q
  (TgProd _ _, TgProd _ _)  -> flatten p `extends` flatten q
  (TgProd _ _, TgAnn _)     -> flatten p `extends` flatten q
  where
    extends Nothing _           = True  --same as rule for `Bot` above
    extends _ Nothing           = False --same as rule for `Bot` above
    extends (Just s1) (Just s2) = all op (S.toList s2) 
      where op s = s `S.member` s1 -- every element in s2 is in s1
                || any (\s' -> isDescendant hier s' s) s1 -- or has a subtag in s1

-- The `isAncestor` and `isDescendant` relations are reflexive,
--  i.e. `isAncestor _ a a = True` for all `a`
isAncestor :: AnnHier -> Ann -> Ann -> Bool
isAncestor hier a1 a2 = isDescendant hier a2 a1

isDescendant :: AnnHier -> Ann -> Ann -> Bool
isDescendant hier a1 a2 = search a2 [a1]
  where search tgt srcs = any (find tgt) srcs
          where find tgt src  
                  | tgt == src = True
                  | otherwise  = search tgt (maybe [] id $ M.lookup src hier)

-- | Generates `:-:` (intersection)
--     based on a hierarchy obtained from annotation declarations
gIntersectionOp :: AnnHier -> TypeOp Tag
gIntersectionOp hier p q = case (p,q) of
  (Bot, _)                    -> q
  (_, Bot)                    -> p
  (_, Top)                    -> Top
  (Top, _)                    -> Top
-- find in the hierarchy the "deepest" `a0` 
--    such that `a1` and `a2` are both descendents of `a1`
--    it may be that `a0 = a1` or that `a0 = a2`
  (TgAnn a1, TgAnn a2)        -> search a1 a2
  (TgAnn a1, TgProd _ _) 
    | a1 `S.member` flatten q -> TgAnn a1
    | otherwise               -> Top
  (TgProd _ _, TgAnn a2) 
    | a2 `S.member` flatten p -> TgAnn a2
    | otherwise               -> Top
  (TgProd _ _, TgProd _ _)  
    | S.null common           -> Top
    | otherwise               -> foldr1 TgProd (map TgAnn $ S.toList common)
        where common = flatten p `S.intersection` flatten q
  -- inefficient but relatively easy to understand
  where
    flatten  = flattenWithDef all_anns 
    all_anns = S.fromList (concat (M.keys hier : M.elems hier))
    search a1 a2 = case deepest of 
      []    -> Top -- if there is no deepest common ancestor
      [a0]  -> TgAnn a0
      _     -> error "multiple deepest common ancestors for simple annotation? :S"
      where mutual = [ a0  | a0 <- S.toList all_anns
                           , isAncestor hier a0 a1
                           , isAncestor hier a0 a2 ]
            deepest = filter op mutual 
              where op a = all (\a' -> not (isAncestor hier a a')) mutual

gCutOp :: AnnHier -> TypeOp Tag
gCutOp hier p q = case (p,q) of
  (Bot, _) | S.null cutoff      -> Top  -- cuts of everything
           | otherwise          -> foldr1 TgProd (map TgAnn $ S.toList cutoff)
                                        -- retains all annotations not in `q`
    where cutoff = all_anns `S.difference` flatten q
  (_, Bot)                      -> Top  -- cuts of everything
  (_, Top)                      -> p    -- cuts of nothing
  (Top, _)                      -> Top  -- nothing to cut off
  (TgAnn a1, TgAnn a2) 
    | a1 == a2                  -> Top -- cuts of everything
    | otherwise                 -> p   -- cuts of nothing
  (TgAnn a1, TgProd _ _) 
    | a1 `S.member` flatten q   -> Top -- ...
    | otherwise                 -> p
  (TgProd _ _, TgAnn a2)
    | S.null cutoff             -> Top
    | otherwise                 -> foldr1 TgProd (map TgAnn $ S.toList cutoff)
   where flat_p = flatten p
         cutoff = flat_p `S.difference` (S.singleton a2)
  (TgProd _ _, TgProd _ _)
    | S.null cutoff             -> Top
    | otherwise                 -> foldr1 TgProd (map TgAnn $ S.toList cutoff)
    where cutoff = flatten p `S.difference` flatten q
  where
    flatten  = flattenWithDef all_anns 
    all_anns = S.fromList (concat (M.keys hier : M.elems hier))


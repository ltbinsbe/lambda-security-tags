
module Semantics.Operators where

import Types.Shared
import Types.Plain

import Data.List (nub, elem)
import qualified Data.Map as M
import qualified Data.Set as S

-- `a1` -> `a2` means `a2` inherits from `a1` (i.e. `a2` is more specific)
type AnnHier  = M.Map Ann [Ann] 
type TagOp a = Tag -> Tag -> a

gHierarchy :: [Decl] -> AnnHier
gHierarchy ds = foldr op M.empty ds
         where op (SecAnn a) = M.insertWith (++) a []
               op (SecAnnExt a2 a1) = M.insertWith (++) a1 [a2]

-- | Generates `<:` based on a hierarchy obtained from annotation declarations
gSubTagOp :: AnnHier -> TagOp Bool
gSubTagOp hier p q = case (p,q) of
  (_, Top)                  -> True   -- implies Top <: Top
  (Top, _)                  -> False
  -- Find in the hierarchy whether `a2` is `a1` or any of the descendents of `a1`
  (TgAnn a1, TgAnn a2)      -> isDescendant hier a1 a2
  (TgAnn _, TgProd _)     -> flatten hier p `extends` flatten hier q
  (TgProd _, TgProd _)  -> flatten hier p `extends` flatten hier q
  (TgProd _, TgAnn _)     -> flatten hier p `extends` flatten hier q
  where
    extends s1 s2 = all op (S.toList s2) 
      where op s = s `S.member` s1 -- every element in s2 is in s1
                || any (\s' -> isDescendant hier s' s) s1 -- or has a subtag in s1

-- The `isAncestor` and `isDescendant` relations are reflexive,
--  i.e. `isAncestor _ a a = True` for all `a`
isDescendant :: AnnHier -> Ann -> Ann -> Bool
isDescendant hier a1 a2 = isAncestor hier a2 a1

isAncestor :: AnnHier -> Ann -> Ann -> Bool
isAncestor hier a1 a2 = search a2 [a1]
  where search tgt srcs = any (find tgt) srcs
          where find tgt src  
                  | tgt == src = True
                  | otherwise  = search tgt (maybe [] id $ M.lookup src hier)

parentOf :: AnnHier -> Ann -> Tag 
parentOf hier chd = 
  case [ par | (par, chds) <- M.assocs hier
             , chd `elem` chds ] of
    []  -> Top
    [a] -> TgAnn a
    _   -> error "parentOf (multiple inheritances)"

-- | Generates intersection
--     based on a hierarchy obtained from annotation declarations
gIntersectionOp :: AnnHier -> TagOp Tag
gIntersectionOp hier p q = case (p,q) of
  (_, Top)                    -> Top
  (Top, _)                    -> Top
-- find in the hierarchy the "deepest" `a0` 
--    such that `a1` and `a2` are both descendents of `a1`
--    it may be that `a0 = a1` or that `a0 = a2`
  (TgAnn a1, TgAnn a2)        -> search a1 a2
  (TgAnn a1, TgProd _)
    | any (isDescendant hier a1) (flatten hier q) -> TgAnn a1
    | otherwise               -> Top
  (TgProd _, TgAnn a2)
    | any (isDescendant hier a2) (flatten hier p) -> TgAnn a2
    | otherwise               -> Top
  (TgProd _, TgProd _)
    | S.null common           -> Top
    | otherwise               -> TgProd $ S.toList common
        where common = flatten hier p `S.intersection` flatten hier q
  -- inefficient but relatively easy to understand
  where
    all_anns = S.fromList (M.keys hier)
    search a1 a2 = case deepest hier mutual of 
      []    -> Top -- if there is no deepest common ancestor
      [a0]  -> TgAnn a0
      _     -> error "multiple deepest common ancestors for simple annotation? :S"
      where mutual = [ a0  | a0 <- S.toList all_anns
                           , isAncestor hier a0 a1
                           , isAncestor hier a0 a2 ]
    searchV a1s a2s 
      | null inter = Top
      | otherwise    = TgProd (nub inter)
      where inter  = [ if p1 then a1 else a2   
                     | a1 <- a1s
                     , a2 <- a2s
                     , let p1 = isAncestor hier a1 a2 
                     , let p2 = isAncestor hier a2 a1
                     , p1 || p2
                     ]

gCutOp :: AnnHier -> TagOp Tag
gCutOp hier p q = case (p,q) of
  (_, Top)                      -> Top  -- cuts of everything 
  (Top, _)                      -> Top  -- nothing to cut off
  (TgAnn a1, TgAnn a2) 
    | gSubTagOp hier p q        -> parentOf hier a2
    | otherwise                 -> p    -- cuts of nothing
  (TgAnn a1, TgProd _) 
    | a1 `S.member` flatten hier q   -> Top -- ...
    | otherwise                 -> p
  (TgProd _, TgAnn a2)
    | S.null cutoff             -> Top
    | otherwise                 -> TgProd $ S.toList cutoff
   where flat_p = flatten hier p
         cutoff = flat_p `S.difference` (S.singleton a2)
  (TgProd _, TgProd _)
    | S.null cutoff             -> Top
    | otherwise                 -> TgProd $ S.toList cutoff
    where cutoff = flatten hier p `S.difference` flatten hier q
  where
    all_anns = S.fromList (M.keys hier)

-- | Flattens a (product) tag to a set of annotations 
-- under the assumption that `t1 * Top` is `t1` for all `t1`
-- the set is such that if `a1 <: a2` then `a2` is in and not `a1`
--                                      or neither is in
flatten :: AnnHier -> Tag -> S.Set Ann
flatten hier (TgProd as)  = S.fromList $ as 
flatten hier Top          = S.empty 
flatten hier (TgAnn ann)  = S.singleton ann

-- | Filters a list of annotations by removing all parents of annotations
-- present in the list (only orphans and children are preserved).
deepest :: AnnHier -> [Ann] -> [Ann]
deepest hier as = filter op as
  where op a = all (\a' -> a' == a || not (isAncestor hier a a')) as


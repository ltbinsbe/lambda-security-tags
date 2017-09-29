
module Semantics.Operators where

import Types.Shared
import Types.Plain

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
  (TgAnn _, TgProd _ _)     -> flatten p `extends` flatten q
  (TgProd _ _, TgProd _ _)  -> flatten p `extends` flatten q
  (TgProd _ _, TgAnn _)     -> flatten p `extends` flatten q
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
    all_anns = S.fromList (M.keys hier)
    search a1 a2 = case deepest of 
      []    -> Top -- if there is no deepest common ancestor
      [a0]  -> TgAnn a0
      _     -> error "multiple deepest common ancestors for simple annotation? :S"
      where mutual = [ a0  | a0 <- S.toList all_anns
                           , isAncestor hier a0 a1
                           , isAncestor hier a0 a2 ]
            deepest = filter op mutual 
              where op a = all (\a' -> a' == a || not (isAncestor hier a a')) mutual

gCutOp :: AnnHier -> TagOp Tag
gCutOp hier p q = case (p,q) of
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
    all_anns = S.fromList (M.keys hier)



module Semantics.Static where

import Types.Shared
import Types.Labelled

import qualified Data.Map as M
import Data.Set (isSubsetOf)
import Data.List (nub)

-- `a1` -> `a2` means `a2` inherits from `a1` (i.e. `a2` is more specific)
type AnnHier  = M.Map Ann [Ann] 
type TypeOp a = Tag -> Tag -> a -- answers `a1` inherits from `a2`?

-- | Generates `<:` based on a hierarchy obtained from annotation declarations
gSubtypeOp :: AnnHier -> TypeOp Bool
gSubtypeOp hier p q = case (p,q) of
  (Bot, _)                  -> True   -- implies Bot <: Bot
  (_, Bot)                  -> False
  (_, Top)                  -> True   -- implies Top <: Top
  (Top, _)                  -> False
  -- Find in the hierarchy whether `a2` is `a1` or any of the descendents of `a1`
  (TgAnn a1, TgAnn a2)      -> isDescendant hier a1 a2
  (TgAnn _, TgProd _ _)     -> flatten p `isContained` flatten q
  (TgProd _ _, TgProd _ _)  -> flatten p `isContained` flatten q
  (TgProd _ _, TgAnn _)     -> flatten p `isContained` flatten q
  where
    isContained Nothing _           = True  --same as rule for `Bot` above
    isContained _ Nothing           = False --same as rule for `Bot` above
    isContained (Just s1) (Just s2) = s1 `isSubsetOf` s2

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
  (Bot, _)                  -> q
  (_, Bot)                  -> p
  (_, Top)                  -> Top
  (Top, _)                  -> Top
-- find in the hierarchy the "deepest" `a0` 
--    such that `a1` and `a2` are both descendents of `a1`
--    it may be that `a0 = a1` or that `a0 = a2`
  (TgAnn a1, TgAnn a2)      -> search a1 a2
  (TgAnn _, TgProd _ _)     -> Top  -- TODO, need to discuss
  (TgProd _ _, TgAnn _)     -> Top  -- TODO, need to discuss
  (TgProd _ _, TgProd _ _)  -> Top  -- TODO, need to discuss
  -- inefficient but relatively easy to understand
  where
    search a1 a2 = case deepest of 
      []    -> Top -- if there is no deepest common ancestor
      [a0]  -> TgAnn a0
      _     -> error "multiple deepest common ancestors for simple annotation? :S"
      where anns   = nub (concat (M.keys hier : M.elems hier))
            mutual = [ a0  | a0 <- anns
                           , isAncestor hier a0 a1
                           , isAncestor hier a0 a2 ]
            deepest = filter op mutual 
              where op a = all (\a' -> not (isAncestor hier a a')) mutual

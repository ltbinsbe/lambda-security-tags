
module Types.Shared where

import Data.Set (Set, fromList)

type Ann      = String  --annotations
type Var      = String  --identifiers/variables
type Ref      = String  --mutable references

data Decl     = SecAnn Ann
              | SecAnnExt Ann Ann

data Tag      = TgAnn Ann
              | TgProd Tag Tag
              | Top
              | Bot
              deriving (Eq)

data Type     = TyArrow Type Type Tag 
              | TyBool Tag
              | TyInt Tag
              | TyRef Type Tag
              | TyArray Type Tag
              deriving (Eq)

isBool (TyBool _) = True
isBool _ = False

typeEq :: Type -> Type -> Bool
typeEq t1 t2 = case (t1,t2) of
  (TyArrow p1 q1 _, TyArrow p2 q2 _)  -> p1 `typeEq` p2 && q1 `typeEq` q2
  (TyArrow _ _ _, _)                  -> False
  (_, TyArrow _ _ _)                  -> False
  (TyBool _, TyBool _)                -> True
  (TyBool _, _)                       -> False
  (_, TyBool _)                       -> False
  (TyInt _, TyInt _)                  -> True
  (TyInt _, _)                        -> False
  (_, TyInt _)                        -> False
  (TyRef p1 _, TyRef p2 _)            -> p1 `typeEq` p2
  (TyRef _ _, _)                      -> False
  (_, TyRef _ _)                      -> False
  (TyArray p1 _, TyArray p2 _)        -> p1 `typeEq` p2

-- | Flattens a (product) tag to a set of annotations 
-- Occurrences of `Bot` and `Top` are filtered 
-- under the assumption that `t1 * Bot` is `Bot` for all `t1`
-- and that `t1 * Top` is `t1` for all `t1`
-- Since `Bot` is the most specific tag
-- The Maybe monad is just to capture occurrences of `Bot`
--   i.e. `flatten t1 = Nothing` infers the product `t1` contains `Bot`
flatten :: Tag -> Maybe (Set Ann)
flatten t = fromList <$> flatten' t 
 where
    flatten' :: Tag -> Maybe [Ann]
    flatten' (TgProd t1 t2)  = (++) <$> flatten' t1 <*> flatten' t2
    flatten' Top             = return []
    flatten' Bot             = Nothing
    flatten' (TgAnn ann)     = return [ann]

-- | Version of `flatten` that replaces `Bot` with the given set
flattenWithDef :: Set Ann -> Tag -> Set Ann
flattenWithDef def = maybe def id . flatten 

tagOf :: Type -> Tag
tagOf ty = case ty of
  TyArrow _ _ tag -> tag
  TyBool tag      -> tag
  TyInt tag       -> tag
  TyRef _ tag     -> tag
  TyArray _ tag   -> tag

replaceTag :: Tag -> Type -> Type
replaceTag tag ty = case ty of 
  TyArrow p q _ -> TyArrow p q tag
  TyBool _      -> TyBool tag
  TyInt _       -> TyInt tag
  TyRef r _     -> TyRef r tag
  TyArray r _   -> TyArray r tag


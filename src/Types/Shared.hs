
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

data Type     = TyArrow Type Type Tag 
              | TyBool Tag
              | TyInt Tag
              | TyRef Type Tag
              | TyArray Type Tag

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

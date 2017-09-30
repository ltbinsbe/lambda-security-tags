
module Types.Shared where

type Ann      = String  --annotations
type Var      = String  --identifiers/variables
type Ref      = String  --mutable references

data Decl     = SecAnn Ann
              | SecAnnExt Ann Ann

data Tag      = TgAnn Ann
              | TgProd [Ann]
              | Top

data Type     = TyArrow Type Type Tag 
              | TyBool Tag
              | TyInt Tag
              | TyRef Type Tag
              | TyArray Type Tag

annotationsOf :: Tag -> [Ann]
annotationsOf (TgAnn ann) = [ann]
annotationsOf Top         = []
annotationsOf (TgProd as) = as

tg_prod :: Tag -> Tag -> Tag
tg_prod t1 t2 = safe_product (annotationsOf t1 ++ annotationsOf t2)

safe_product :: [Ann] -> Tag
safe_product as = case as of
  []  -> Top
  [a] -> TgAnn a
  as  -> TgProd as

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


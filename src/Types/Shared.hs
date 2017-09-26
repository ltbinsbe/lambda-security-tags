
module Types.Shared where

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


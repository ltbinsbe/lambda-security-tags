
module Types.EDSL where

import Prelude hiding ((<*>), (<$>))

import Types.Shared
import Types.Plain

ann_decl :: Ann -> Decl
ann_decl = SecAnn

extends :: Ann -> Ann -> Decl
extends = SecAnnExt

var :: Var -> Term
var = TVar

infix 5 <:>
(<:>) :: Var -> Type -> (Var, Type)
v <:> ty = (v,ty)

infix 4 <.>
(<.>) :: (Var, Type) -> Term -> Term
(v,ty) <.> t = TLam v ty t Top

infix 3 <$>
(<$>) :: Term -> Term -> Term
(<$>) = TApp

infix 5 <=> 
(<=>) :: Var -> Term -> (Var, Term)
v <=> t = (v,t)

infix 4 <..>
(<..>) :: (Var, Term) -> Term -> Term
(<..>) = let_in

let_in :: (Var, Term) -> Term -> Term
(v,t) `let_in` b = TLet v t b 

as :: Term -> Tag -> Term
as = TAs

drop :: Term -> Tag -> Term
drop = TDrop

copy :: Term -> Term -> Term
copy = TCopy

if_then_else :: Term -> Term -> Term -> Term
if_then_else = TITE

-- building literals

true :: Term
true = TBool True Top

false :: Term
false = TBool False Top

q :: Int -> Term
q = flip TInt Top

-- building tags

ann :: Ann -> Tag
ann = TgAnn
top = Top

-- building types

boolean :: Tag -> Type
boolean = TyBool

integer :: Tag -> Type
integer = TyInt

infixr 5 --> --type arrow
(-->) :: Type -> Type -> Type
p --> q = TyArrow p q Top

ex1 :: Program 
ex1 = Program [
    ann_decl "CSRV"
  , ann_decl "CryptKey"
  ]  
  ("x" <:> boolean (ann "CSRV") <.> var "x" `as` (ann "CryptKey" ))
  Nothing

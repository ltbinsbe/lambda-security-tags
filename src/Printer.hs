
module Printer (ppProgram) where

import Types.Shared
import Types.Labelled

import Text.PrettyPrint.HughesPJ

instance Show Program where
  show = render . ppProgram

ppProgram :: Program -> Doc
ppProgram (Program ds t) = vcat (map ppDecl ds) $$ ppTerm t

ppDecl :: Decl -> Doc
ppDecl d = case d of
  SecAnn ann    -> text "SecAnn" <+> text ann 
  SecAnnExt ann1 ann2 -> text "SecAnn" <+> text ann1 <+> text "extends" <+> text ann2

ppTerm :: Term -> Doc
ppTerm t = case t of
  TVar v l -> label l (text v)
  TApp p q l -> label l (ppTerm p <+> ppTerm q)
  TITE g p q l -> label l $ text "if" <+> ppTerm g <+> text "then" <+> ppTerm p 
                                                  <+> text "else" <+> ppTerm q
  TLet v l1 p q l -> label l $ text "let" <+> label l1 (text v) <+> text "="
                           <+> ppTerm p <+> text "in" <+> ppTerm q
  TAs term tag l  -> label l (ppTerm term <+> text "as" <+> ppTag tag)
  TDrop term tag l-> label l (ppTerm term <+> text "drop" <+> ppTag tag)
  TLam v l1 ty t l -> label l $ text "\\" <> label l1 (text v) <+> text ":" 
                            <+> ppType ty <+> text "." <+> ppTerm t
  TBool True l      -> label l (text "true")
  TBool False l     -> label l (text "false")
  TInt q l          -> label l (text (show q))
  TRef r l          -> text "<REF>"
  TArray ts l       -> text "<ARRAY>"
 
ppType :: Type -> Doc
ppType ty = case ty of
  TyArrow p q t   -> ppType p <+> text "->" <+> ppType q <> ppTag t
  TyBool t        -> text "Bool" <> ppTag t
  TyInt t         -> text "Int" <> ppTag t
  TyRef ty tag    -> parens (text "Ref" <+> ppType ty) <> ppTag tag
  TyArray ty tag  -> parens (text "Array" <+> ppType ty) <> ppTag tag

-- version of ppTag' placing enclosing surrounding angle brackets
ppTag :: Tag -> Doc
ppTag ta = angles $ ppTag' ta

ppTag' :: Tag -> Doc
ppTag' ta = case ta of
        TgAnn ann   -> text ann
        TgProd p q  -> ppTag' p <+> text "*" <+> ppTag' q
        Top         -> text "TOP"
        Bot         -> text "BOT"

angles :: Doc -> Doc
angles d = text "<" <> d <> text ">"

label :: Label -> Doc -> Doc
label l term = text ("[" ++ show l ++"|") <> term <> text "|]"


module Printer (ppProgram) where

import Types.Shared
import Types.Plain

import Text.PrettyPrint.HughesPJ

import qualified Data.Set as S

instance Show Program where
  show = render . ppProgram

instance Show Type where
  show = render . ppType

instance Show Term where
  show = render . ppTerm

ppProgram :: Program -> Doc
ppProgram (Program ds t) = vcat (map ppDecl ds) $$ ppTerm t

ppDecl :: Decl -> Doc
ppDecl d = case d of
  SecAnn ann    -> text "SecAnn" <+> text ann 
  SecAnnExt ann1 ann2 -> text "SecAnn" <+> text ann1 <+> text "extends" <+> text ann2

ppTerm :: Term -> Doc
ppTerm t = case t of
  TVar v -> text v
  TApp p q -> parens (ppTerm p <+> ppTerm q)
  TITE g p q -> text "if" <+> ppTerm g <+> text "then" <+> ppTerm p 
                                                  <+> text "else" <+> ppTerm q
  TLet v p q -> text "let" <+> text v <+> text "="
                           <+> ppTerm p <+> text "in" <+> ppTerm q
  TAs term tag    -> parens (ppTerm term <+> text "as" <+> ppTag tag)
  TDrop term tag  -> parens (ppTerm term <+> text "drop" <+> ppTag tag)
  TLam v ty t tag -> parens (text "\\" <> text v <+> text ":" 
                            <+> ppType ty <+> text "." <+> ppTerm t)
                            <> ppFlattened tag
  TBool True tag  -> text "true" <> ppFlattened tag
  TBool False tag -> text "false" <> ppFlattened tag
  TInt q tag      -> text (show q) <> ppFlattened tag
  TRef r tag      -> text "<REF>" <> ppFlattened tag
  TArray ts tag   -> text "<ARRAY>" <> ppFlattened tag

ppFlattened :: Tag -> Doc
ppFlattened tag | null flat = empty
                | otherwise = angles (csd (map text flat)) 
  where flat = maybe (error "ppFlattened Bottom") S.toList $ flatten tag
 
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
        Top         -> empty 
        Bot         -> text "BOT"

angles :: Doc -> Doc
angles d = text "<" <> d <> text ">"

csd :: [Doc] -> Doc
csd = hcat . punctuate comma



module Parser where

import Types.Shared
import Types.Plain

import GLL.Combinators

lexerSettings = emptyLanguage {
    keywords =  ["this", "let", "in", "if", "then", "else", "as", "drop"
                ,"SecAnn", "extends", "true", "false", "Bool", "Int"]
  , keychars = ['=', '\\', '.', '(', ')', '!', '*', '<', '>', ':']
  }

type Parser a = BNF Token a

parser :: [Token] -> [Either String Program]
parser = parseWithParseOptions [] [maximumErrors 1, throwErrors]
  (Right <$$> pProgram)

pProgram :: Parser Program
pProgram = "programs" 
  <:=> Program <$$> multiple pDecl <**> pTerm <**>
    optional (keychar ':' **> pType) -- typing the check by static analyser

pDecl :: Parser Decl
pDecl = "declarations"
  <:=> SecAnn <$$ keyword "SecAnn" <**> pAnn 
  <||> SecAnnExt <$$ keyword "SecAnn" <**> pAnn <** keyword "extends" <**> pAnn

pTerm :: Parser Term
pTerm = "terms"
  <::= TVar <$$> pVar
  <||> TApp <$$> pTerm <**>>> pTerm --left associative
  <||> TITE <$$ keyword "if" <**> pTerm <** keyword "then" <**> pTerm 
                                        <** keyword "else" <**> pTerm
  <||> TLet <$$ keyword "let" <**> pVar <** keychar '=' <**> pTerm 
                                        <** keyword "in" <**> pTerm
  <||> TAs  <$$> pTerm <** keyword "as" <**> pTag
  <||> TDrop <$$> pTerm <** keyword "drop" <**> pTag
  <||> parens (TLam <$$ keychar '\\' <**> pVar <** keychar ':' <**> pType
                                              <** keychar '.' <**> pTerm)
                                       <**> optionalWithDef pTag Top
  <||> TBool True <$$ keyword "true" <**> optionalWithDef pTag Top
  <||> TBool False <$$ keyword "false" <**> optionalWithDef pTag Top
  <||> TInt <$$> int_lit <**> optionalWithDef pTag Top
  <||> parens pTerm

pType :: Parser Type
pType = "types"
  <:=> TyBool <$$ keyword "Bool" <**> optionalWithDef pTag Top
  <||> TyInt  <$$ keyword "Int" <**> optionalWithDef pTag Top
  <||> parens (TyArrow <$$> pType <** keyword "->" <**>>> pType) --left associative
          <**> pTag

pTag :: Parser Tag
pTag = "tags" <:=> angles pTagContents

pTagContents :: Parser Tag
pTagContents = "contents-tags" 
  <::=  TgAnn <$$> pAnn
  <||>  satisfy Top -- consumes no tokens
  <||>  TgProd <$$> manySepBy2 pAnn (keychar '*') 

pVar :: Parser Var
pVar = "variables" <:=> id_lit

pAnn :: Parser Ann
pAnn = "annotation" <:=> alt_id_lit

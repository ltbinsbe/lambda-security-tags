
module Semantics.Labelling where

import Types.Shared
import qualified Types.Plain as P
import qualified Types.Labelled as L

import Control.Monad.State

import qualified Data.Map as M

type Labeller a = M.Map Var L.Label -> State L.Label a

get_label :: State L.Label L.Label 
get_label = do  l <- get
                put (l + 1)
                return l 

process :: P.Program -> L.Program
process (P.Program ds term) = L.Program ds $ evalState (tTerm term M.empty) 1

tTerm :: P.Term -> Labeller L.Term
tTerm t env = case t of
  P.TVar v -> case M.lookup v env of
                Nothing -> L.TVar v <$> get_label 
                Just l  -> return (L.TVar v l)
  P.TApp p q -> L.TApp <$> tTerm p env <*> tTerm q env <*> get_label
  P.TITE p q r -> L.TITE <$> tTerm p env <*> tTerm q env <*> tTerm r env <*> get_label
  P.TLet var p q -> do 
    l1 <- get_label
    let env' = M.insert var l1 env
    L.TLet var l1 <$> tTerm p env {- let not recursive -} <*> tTerm q env' <*> get_label
  P.TAs t tag -> L.TAs <$> tTerm t env <*> return tag <*> get_label
  P.TDrop t tag -> L.TDrop <$> tTerm t env <*> return tag <*> get_label
  P.TLam var ty t -> do 
    l1 <- get_label
    let env' = M.insert var l1 env
    L.TLam var l1 ty <$> tTerm t env' <*> get_label
  P.TBool bool  -> L.TBool bool <$> get_label
  P.TInt int    -> L.TInt int <$> get_label
  P.TRef ref    -> L.TRef ref <$> get_label
  P.TArray ts   -> L.TArray <$> mapM (flip tTerm env) ts <*> get_label

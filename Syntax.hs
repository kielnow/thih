-----------------------------------------------------------------------------
-- |
-- Module      : Syntax
-- Description : Abstract syntax
-- Copyright   : kielnow, March 31, 2015
-- 
-----------------------------------------------------------------------------
module Syntax where

import Data.List (union,nub,intersect)

-- | Identifiers
type Id = String

-- | Kinds
data Kind = KStar | KFun Kind Kind 
          deriving (Eq, Show)
                   
-- | Types
data Type = TVar Tyvar | TCon Tycon | TApp Type Type | TGen Int
          deriving (Eq, Show)
                   
-- | Type variables
data Tyvar =  Tyvar Id Kind 
           deriving (Eq, Show)

-- | Type constants
data Tycon = Tycon Id Kind 
           deriving (Eq, Show)
                    
-- | Primitive datatypes
tUnit    = TCon (Tycon "()" KStar)
tChar    = TCon (Tycon "Char" KStar)
tInt     = TCon (Tycon "Int" KStar)
tInteger = TCon (Tycon "Integer" KStar)
tFloat   = TCon (Tycon "Float" KStar)
tDouble  = TCon (Tycon "Double" KStar)
tList    = TCon (Tycon "[]" (KFun KStar KStar))
tArrow   = TCon (Tycon "(->)" (KFun KStar (KFun KStar KStar)))
tTuple2  = TCon (Tycon "(,)" (KFun KStar (KFun KStar KStar)))

-- | Synonyms
tString  = list tChar

-----------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------
infixr 4 ~>
(~>) :: Type -> Type -> Type
a ~> b = TApp (TApp tArrow a) b

list :: Type -> Type
list t = TApp tList t

pair :: Type -> Type -> Type
pair a b = TApp (TApp tTuple2 a) b

class HasKind t where
  kind :: t -> Kind
instance HasKind Tyvar where
  kind (Tyvar _ k) = k
instance HasKind Tycon where
  kind (Tycon _ k) = k
instance HasKind Type where
  kind (TVar u) = kind u
  kind (TCon c) = kind c
  kind (TApp t _) | KFun _ k <- kind t = k
  kind _ = error "kind"

-----------------------------------------------------------------------------
-- Substitutions
-----------------------------------------------------------------------------
-- | Substitutions
type Subst = [(Tyvar, Type)]

nullSubst :: Subst
nullSubst = []

infix 4 +->
(+->) :: Tyvar -> Type -> Subst
u +-> t = [(u,t)]

class Types t where
  apply :: Subst -> t -> t
  tv :: t -> [Tyvar]

instance Types Type where
  apply s (TVar u) = case lookup u s of
    Just t -> t
    Nothing -> TVar u
  apply s (TApp l r) = TApp (apply s l) (apply s r)
  apply _ t = t
  
  tv (TVar u) = [u]
  tv (TApp l r) = tv l `union` tv r
  tv _ = []

instance Types a => Types [a] where
  apply s = map (apply s)
  tv = nub . concat . map tv

-- apply (s1 @@ s2) == apply s1 . apply s2
infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u,t) <- s2] ++ s1

merge :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return (s1++s2) else fail "merge fails"
  where agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v)) (map fst s1 `intersect` map fst s2)


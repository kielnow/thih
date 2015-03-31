-----------------------------------------------------------------------------
-- |
-- Module      : Syntax
-- Description : Abstract syntax
-- Copyright   : kielnow, March 31, 2015
-- 
-----------------------------------------------------------------------------
module Syntax where

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
(~>) a b = TApp (TApp tArrow a) b

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
  kind (TVar tv) = kind tv
  kind (TCon tc) = kind tc
  kind (TApp t _) | KFun _ k <- kind t = k
  kind _ = undefined
  
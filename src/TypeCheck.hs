module TypeCheck where

import qualified Data.Map.Strict as Map
import LambdaTerm
import TypeTerm

typeOf :: Const -> TypeTerm
typeOf Unit = TypeConstant UnitType
typeOf (Integer _) = TypeConstant IntegerType
typeOf (Boolean _) = TypeConstant BooleanType
typeOf Addition = TypeFunction (TypeConstant IntegerType) (TypeFunction (TypeConstant IntegerType) (TypeConstant IntegerType))
typeOf Multiplication = TypeFunction (TypeConstant IntegerType) (TypeFunction (TypeConstant IntegerType) (TypeConstant IntegerType))
typeOf Or = TypeFunction (TypeConstant BooleanType) (TypeFunction (TypeConstant BooleanType) (TypeConstant BooleanType))
typeOf And = TypeFunction (TypeConstant BooleanType) (TypeFunction (TypeConstant BooleanType) (TypeConstant BooleanType))

{-# LANGUAGE TypeFamilies #-}

module DataReifyPrototype (dataReifyNodeCount) where

import Data.Reify (Graph (..), MuRef (..), reifyGraph)

import Vpipe.Expr.Internal (Expr (..), ExprNode (..), ExprObject (..), SomeExpr (..))

data PrototypeExpression child
  = PrototypeLiteral
  | PrototypeBinary child child

newtype ExpressionReference = ExpressionReference ExprObject

instance MuRef ExpressionReference where
  type DeRef ExpressionReference = PrototypeExpression

  mapDeRef visit (ExpressionReference object) = case objectNode object of
    LiteralNode{} -> pure PrototypeLiteral
    BinaryNode _ left right -> PrototypeBinary <$> visit (ExpressionReference (expressionObject left)) <*> visit (ExpressionReference (expressionObject right))
    _ -> error "data-reify sharing prototype received a node outside its literal/binary benchmark domain"

dataReifyNodeCount :: SomeExpr -> IO Int
dataReifyNodeCount expression = do
  Graph nodes _ <- reifyGraph (ExpressionReference (expressionObject expression))
  pure (length nodes)

expressionObject :: SomeExpr -> ExprObject
expressionObject (SomeExpr (Expr object)) = object

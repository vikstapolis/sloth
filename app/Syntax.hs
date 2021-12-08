module Syntax where

import DataStructs.Vector (Vector)
import qualified DataStructs.Vector as V

data Literal = BoolL   Bool
             | StringL String
             | IntL    Integer
             | FloatL  Float
             | ListL   (Vector Literal)
             | FuncL   [String] Block
             | ExternL [String] ([Literal] -> Literal)
             | VarL    String
             | VoidL
             
instance Show Literal where
    show (BoolL b)   = "BoolL " ++ show b
    show (StringL s) = "StringL " ++ show s
    show (IntL i)    = "IntL " ++ show i
    show (FloatL f)  = "FloatL " ++ show f
    show (ListL l)   = "ListL " ++ show (V.length l)

instance Eq Literal where
    (BoolL b)   == (BoolL c)   = b == c
    (StringL s) == (StringL t) = s == t
    (IntL i)    == (IntL j)    = i == j
    (IntL i)    == (FloatL f)  = fromIntegral i == f
    (FloatL f)  == (FloatL g)  = f == g
    (FloatL f)  == (IntL i)    = f == fromIntegral i
    (ListL _)   == (ListL _)   = error "Cannot compare lists with ==, use \ 
                                       \`eq` function instead"
    _           == _           = error "Invalid comparison"

instance Ord Literal where
    (BoolL b)   `compare` (BoolL c)   = b `compare` c
    (StringL s) `compare` (StringL t) = s `compare` t
    (IntL i)    `compare` (IntL j)    = i `compare` j
    (IntL i)    `compare` (FloatL f)  = fromIntegral i `compare` f
    (FloatL f)  `compare` (FloatL g)  = f `compare` g
    (FloatL f)  `compare` (IntL i)    = f `compare` fromIntegral i
    (ListL _)   `compare` (ListL _)   = error "Cannot compare lists with < or >, use \ 
                                       \`lt` and `gt` functions instead"
    _           `compare` _           = error "Invalid comparison"


data Expression = LitE  Literal
                | OpE   Operator Expression Expression
                | CallE String [Expression]
                deriving (Show)


data Operator = PlusO
              | MinusO
              | MultiplyO
              | DivideO
              | EqualO
              | GreaterO
              | LesserO
              | IndexO -- To index a List
              | CustomO String
              deriving (Show)


data Statement = DeclarationS String Expression
               | AssignmentS String (Maybe Operator) Expression
               | IfS Expression Block (Maybe Block)
               | WhileS Expression Block
               | ForeachS String Expression Block
               | ReturnS Expression
               | OutputS Expression
               | InputS String Expression
               deriving (Show)

type Block = [Statement]
module Syntax where

import DataStructs.Vector (Vector, (!))
import qualified DataStructs.Vector as V
import Control.Monad (forM_, foldM)

data Literal = BoolL   Bool
             | StringL String
             | IntL    Integer
             | FloatL  Float
             | ListL   (Vector Literal)
             | FuncL   [String] Block
             | VarL    String
             | VoidL

showLit :: Literal -> IO String
showLit (BoolL b) = return $ show b
showLit (StringL s) = return s
showLit (IntL i) = return $ show i
showLit (FloatL f) = return $ show f
showLit (ListL l) = if len == 0
                        then return "[]"
                        else do last <- l ! (len - 1)
                                str  <- (++"]") <$> showLit last
                                str' <- foldM (\acc i -> do
                                                (++(',':acc)) <$> (l ! i >>= showLit)
                                            ) str [len - 2, len - 3 .. 0]
                                return $ '[' : str'

    where len = V.length l

printLit :: Literal -> IO ()
printLit (BoolL b) = print b
printLit (StringL s) = putStrLn s
printLit (IntL i) = print i
printLit (FloatL f) = print f
printLit (ListL l) = if V.length l == 0
                         then putStrLn "[]"
                         else do
                            putChar '['
                            l ! 0 >>= showLit >>= putStr
                            forM_ [1 .. V.length l - 1] $ \i -> do
                                putChar ','
                                l ! i >>= showLit >>= putStr
                            putStrLn "]"

instance Show Literal where
    show (BoolL b)   = "BoolL " ++ show b
    show (StringL s) = "StringL " ++ show s
    show (IntL i)    = "(IntL " ++ show i ++ ")"
    show (FloatL f)  = "FloatL " ++ show f
    show (ListL l)   = "ListL " ++ show (V.length l)
    show (VarL s)    = "(VarL " ++ show s ++ ")"

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


data Expression = LitE         Literal
                | OpE          Operator Expression Expression
                | CallE        String [Expression]
                | DeclarationE String Expression
                | AssignmentE  String (Maybe Operator) Expression
                | ExternE      [Expression] ([Literal] -> IO Literal)
            
instance Show Expression where
    show (LitE l) = show l
    show (OpE op e1 e2) = "OpE " ++ show op ++ " " ++ show e1 ++ " " ++ show e2
    show (CallE call es) = "CallE " ++ call ++ " " ++ show es
    show (DeclarationE decl e) = "DeclE " ++ decl ++ " " ++ show e
    show (AssignmentE name op e) = "AssignmentE " ++ name ++ " " ++ show op ++ " " ++ show e
    show (ExternE es _) = "ExternE " ++ show es


data Operator = PlusO
              | MinusO
              | MultiplyO
              | DivideO
              | ModuloO
              | EqualO
              | GreaterO
              | LesserO
              | IndexO -- To index a List
              | CustomO String
              deriving (Show)


data Statement = IfS Expression Block (Maybe Statement)
               | WhileS Expression Block
               | ForeachS String Expression Block
               | ReturnS Expression
               | OutputS Expression
               | InputS String Expression
               | ExpressionS Expression
               deriving (Show)

type Block = [Statement]
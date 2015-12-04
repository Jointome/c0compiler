module Compile where
import Grammar
import Tokens

type Code = (Op, Value, Value, Value)
data Op =  OpIf | OpAt | OpLb
data Value = Exp | UVar String | Label String | TVar(String, Type) | Null deriving(Show)
  -- aqui no value o Exp para ir buscar o Num Int???

new_label :: Int -> String
new_label n = "label" ++ (show n)

new_var :: Int -> String
new_var n = "$t" ++ (show n)


compile_cmd :: Int -> Command -> ([Code], Int)
compile_cmd :: lbNum (Atrib (SVar var) exp) = (expCode ++ [(OpAt, UVar var, UVar expVar, Null)], lbNum)
            where (expVar, expCode) = compile_exp 0 exp
            -- este lbNum, onde e que isto vai ser metido???????


compile_exp :: Int -> Exp -> (String, [Code], Int)
compile_exp nx (Num x) = (var, [(OpAt, UVar var, (Num x), Null)], nx + 1)
            where var = new_var nx

mipscode:: Code -> String
mipscode (OpAt, UVar v, p, Null)  = "       li   " ++ v ++ ", " ++  show p

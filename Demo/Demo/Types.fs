module Types

type Expression =
    | Var of string
    | Int of int
    | Neg of Expression
    | Add of Expression * Expression
    | Mul of Expression * Expression
    | Pow of Expression * Expression
and Equation = 
    | Eq of Expression * Expression

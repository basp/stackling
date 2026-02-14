module Stackling.Builtins

open Stackling.Runtime

let dup rt : Result<Runtime, JoyError> =
    match rt.Stack with
    | x :: xs ->
        Ok { rt with Stack = x :: x :: xs }
    | _ ->
        Error StackUnderflow

let swap rt =
    match rt.Stack with
    | x :: y :: xs ->
        Ok { rt with Stack = y :: x :: xs }
    | _ ->
        Error StackUnderflow
    
let pop rt =
    match rt.Stack with
    | _ :: xs ->
        Ok { rt with Stack = xs }
    | _ ->
        Error StackUnderflow
        
let i rt =        
    match rt.Stack with
    | Quotation q :: xs ->
        Ok { rt with
                Stack = xs
                Queue = q @ rt.Queue }
    | x :: _ ->
        Error (InvalidQuotation x)
    | [] ->
        Error StackUnderflow
        
let private intBinOp name op a b rt =
    Ok { rt with Stack = Int (op a b) :: rt.Stack }

let private floatBinOp name op a b rt =
    Ok { rt with Stack = Float (op a b) :: rt.Stack }

let private numericBinOp name intOp floatOp rt =
    match rt.Stack with
    | Int b :: Int a :: _ ->
        intBinOp name intOp a b rt
    | Float b :: Float a :: _ ->
        floatBinOp name floatOp a b rt
    | _ ->
        Error (TypeError "Cannot operate on non-numeric values")        

let add rt =
    numericBinOp "add" (+) (+) rt

let sub rt =
    numericBinOp "sub" (-) (-) rt

let mul rt =
    numericBinOp "mul" (*) (*) rt

let div rt =
    match rt.Stack with
    | Int 0 :: _ :: _ ->
        Error DivisionByZero
    | Float 0.0 :: _ :: _ ->
        Error DivisionByZero
    | _ ->
        numericBinOp "div" (/) (/) rt
        
let private builtins =
    Map.empty
    |> Map.add Dup dup
    |> Map.add Swap swap
    |> Map.add Pop pop
    |> Map.add I i
    |> Map.add Add add
    |> Map.add Sub sub
    |> Map.add Mul mul
    |> Map.add Div div
    
let tryFindBuiltin sym =
    builtins |> Map.tryFind sym
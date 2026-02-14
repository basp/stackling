module Stackling.Builtins

open Stackling.Runtime

// type BinOp =
//     | IntBinOp of (int -> int -> int)
//     | FloatBinOp of (float -> float -> float)
//     
// type UnOp =
//     | IntUnOp of (int -> int)
//     | FloatUnOp of (float -> float)

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
        
let private intBinOp _ op a b rest rt =
    Ok { rt with Stack = Int (op a b) :: rest }

let private floatBinOp _ op a b rest rt =
    Ok { rt with Stack = Float (op a b) :: rest }

let private numericBinOp name intOp floatOp rt =
    match rt.Stack with
    | Int b :: Int a :: rest ->
        intBinOp name intOp a b rest rt
    | Float b :: Float a :: rest ->
        floatBinOp name floatOp a b rest rt
    | _ ->
        let msg = sprintf "%s expects same-type numeric values" name
        Error (TypeError msg)

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
    |> Map.add Dup {
        Impl = dup
        Effect = ([TAny], [TAny; TAny])
    }
    |> Map.add Swap {
        Impl = swap
        Effect = ([TAny; TAny], [TAny; TAny])
    }
    |> Map.add Pop {
        Impl = pop
        Effect = ([TAny], [])
    }
    |> Map.add I {
        Impl = i
        Effect = ([TQuotation], [])
    }
    |> Map.add Add {
        Impl = add
        Effect = ([TNumeric; TNumeric], [TNumeric])
    }
    |> Map.add Sub {
        Impl = sub
        Effect = ([TNumeric; TNumeric], [TNumeric])
    }
    |> Map.add Mul {
        Impl = mul
        Effect = ([TNumeric; TNumeric], [TNumeric])
    }
    |> Map.add Div {
        Impl = div
        Effect = ([TNumeric; TNumeric], [TNumeric])
    }
    
let tryFindBuiltin sym =
    builtins |> Map.tryFind sym
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
        
let private builtins =
    Map.empty
    |> Map.add Dup dup
    |> Map.add Swap swap
    |> Map.add Pop pop
    |> Map.add I i
    
let tryFindBuiltin sym =
    builtins |> Map.tryFind sym
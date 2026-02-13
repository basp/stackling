#load "Runtime.fs"
#load "Builtins.fs"
#load "Diagnostics.fs"
#load "Interpreter.fs"

open Stackling.Runtime
open Stackling.Interpreter
open Stackling.Diagnostics

let p = [Symbol "dup"; Int 2]
let rt = { defaultRuntime with Queue = p }

let x =
    step rt
    |> Result.bind step
    |> Result.bind step
    |> Result.map initRuntimeStateView

let y =
    runUntilHalt rt
    |> Result.map initRuntimeStateView

let testOk =
    match x, y with
    | Ok x, Ok y ->
        x = y
    | Error (x, _, _), Error (y, _, _) ->
        x = y
    | _ -> false
    
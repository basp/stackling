module Stackling.Diagnostics

open Stackling.Runtime

type RuntimeStateView = {
    Queue: JoyValue list
    Stack: JoyValue list
    TraceSummary: JoyValue list }

let initRuntimeStateView (rt: Runtime) =
    let traceSummary =
        rt.Trace
        |> List.map (fun te -> te.Instruction)
        |> List.rev
    { Queue = rt.Queue
      Stack = rt.Stack
      TraceSummary = traceSummary }
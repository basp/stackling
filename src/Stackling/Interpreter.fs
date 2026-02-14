module Stackling.Interpreter

open Stackling.Runtime
open Stackling.Builtins

// On failure, the runtime returned in the error case reflects the state
// after consuming the failing instruction, but before applying any effects.
// The failing instruction is recorded only in the returned TraceEntry.
let step (rt : Runtime) : Result<Runtime, JoyError * TraceEntry * Runtime> =
    match rt.Queue with
    | [] ->
        Ok rt
    | instr :: remainingQueue ->
        // Save a snapshot of the state before executing instruction.
        let stackBefore = rt.Stack
        let queueBefore = rt.Queue

        // Base runtime for the next step.        
        let baseRt = { rt with Queue = remainingQueue }

        // Execute instruction.
        let resultAfter =
            match instr with
            | Int _
            | Bool _
            | String _
            | Quotation _ ->
                // Pushing a literal never fails.
                Ok ({ baseRt with Stack = instr :: stackBefore }, None)
            | Symbol name ->
                // Symbol lookup.
                match rt.Env.TryFind name with
                | None ->
                    Error (UndefinedSymbol name)
                | Some (Builtin sym) ->
                    // Some extra indirection here, but it's worth it.
                    // Possibly clean up the code using active patterns.
                    match tryFindBuiltin sym with
                    | Some bi ->
                        bi baseRt
                        |> Result.map (fun newRt ->
                            (newRt, Some (Builtin sym)))
                    | None ->
                        Error (UndefinedSymbol (sym.ToString()))
                // User-defined words expand into the queue.
                | Some (Defined def) ->
                    let newRt = { baseRt with Queue = def @ baseRt.Queue }
                    Ok (newRt, Some (Defined def))

        match resultAfter with
        | Error err ->
            // Set up the trace entry for the failed instruction.
            // Return separately, don't include it in runtime trace.
            let traceEntry =
                {
                    Instruction = instr
                    StackBefore = stackBefore
                    // Don't touch the runtime stack.
                    StackAfter = rt.Stack
                    QueueBefore = queueBefore
                    // Make sure the failed instruction is not
                    // included in the queue after.
                    QueueAfter = remainingQueue
                    // No resolution - we failed.
                    Resolution = None
                }
            // Return `baseRt` so the failed instruction appears consumed.
            Error (err, traceEntry, baseRt)
        
        // On success, set up the trace entry and return the modified runtime.
        | Ok (rtAfter, resolution) ->
            let traceEntry =
                {
                    Instruction = instr
                    StackBefore = stackBefore
                    StackAfter = rtAfter.Stack
                    QueueBefore = queueBefore
                    QueueAfter = rtAfter.Queue
                    Resolution = resolution
                }
            Ok { rtAfter with Trace = traceEntry :: rt.Trace }

// Halt on the first error.
let rec runUntilHalt (rt: Runtime) = 
    match rt.Queue with
    | [] -> Ok rt
    | _ ->
        match step rt with
        | Ok rt -> runUntilHalt rt
        | err -> err        

let defaultEnv = Map.ofList [
    "dup", Builtin Dup
    "swap", Builtin Swap
    "pop", Builtin Pop
    "i", Builtin I
]

let defaultRuntime =
    { Queue = []
      Stack = []
      Env = defaultEnv
      Trace = [] }

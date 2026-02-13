module Stackling.Tests

open Stackling.Runtime
open Stackling.Interpreter
open Xunit

let expectOk
    (x: Result<Runtime, JoyError * TraceEntry * Runtime>)
    (test: Runtime -> unit)=
        match x with
        | Ok rt -> test rt
        | Error (err, _, _) -> Assert.Fail(err.ToString())

let expectError
    (x: Result<Runtime, JoyError * TraceEntry * Runtime>)
    (test: JoyError * TraceEntry * Runtime -> unit) =
        match x with
        | Ok _ -> Assert.Fail("Expected error")
        | Error err -> test err

[<Fact>]
let ``int literals are pushed onto the stack`` () =
    let rt0 = { defaultRuntime with Queue = [Int 1] }
    expectOk (step rt0) (fun rt ->
        let trace = rt.Trace.Head
        Assert.Equal(Int 1, rt.Stack.Head)
        Assert.Empty(rt.Queue)
        Assert.Equal(None, trace.Resolution))

[<Fact>]
let ``dup should duplicate value on top of stack`` () =
    let rt0 = { defaultRuntime with Queue = [Int 1; Symbol "dup"] }
    // Execute using `step` as a sanity check.
    let rt1 = rt0 |> step |> Result.bind step
    expectOk rt1 (fun rt ->
        match rt.Stack with
        | _ :: _ :: _ ->
            Assert.StrictEqual([Int 1; Int 1], rt.Stack)
        | _ ->
            Assert.Fail("Stack is not of length 2"))
      
[<Fact>]
let ``literal only program`` () =
    let p = [Int 1; Int 2; Int 3]
    let rt0 = { defaultRuntime with Queue = p }
    expectOk (runUntilHalt rt0) (fun rt ->
        match rt.Stack with
        | _ :: _ ::_z :: _ ->
            Assert.StrictEqual([Int 3; Int 2; Int 1], rt.Stack)
        | _ ->
            Assert.Fail("Stack is not of length 3"))

[<Fact>]
let ``builtin only program`` () =
    let p = [Int 1; Symbol "dup"; Symbol "dup"]
    let rt0 = { defaultRuntime with Queue = p }
    expectOk (runUntilHalt rt0) (fun rt ->
        match rt.Stack with
        | _ :: _ :: _ :: _ ->
            Assert.StrictEqual([Int 1; Int 1; Int 1], rt.Stack)
        | _ ->
            Assert.Fail("Stack is not of length 3"))
    
[<Fact>]
let ``user-defined word expansion`` () =
    let foo = Defined [Int 1; Int 2; Symbol "dup"]
    let env = defaultEnv |> Map.add "foo" foo
    let rt0 = { defaultRuntime with Env = env; Queue = [Symbol "foo"] }
    expectOk (runUntilHalt rt0) (fun rt ->
         match rt.Stack with
         | _ :: _ :: _ :: _ ->
             Assert.StrictEqual([Int 2; Int 2; Int 1], rt.Stack)
         | _ ->
             Assert.Fail("Stack is not of length 3"))
    
[<Fact>]
let ``error propagation`` () =
    let p = [Symbol "dup"]
    let rt0 = { defaultRuntime with Queue = p }
    expectError (runUntilHalt rt0) (fun (err, traceEntry, _) ->
        Assert.Equal(Symbol "dup", traceEntry.Instruction)
        Assert.Equal(StackUnderflow, err))
      
[<Fact>]
let ``instruction is consumed on error`` () =
    let p = [Symbol "dup"]
    let rt0 = { defaultRuntime with Queue = p }
    expectError (runUntilHalt rt0) (fun (_, _, rt) ->
        Assert.Empty(rt.Queue))

[<Fact>]
let ``halt on empty queue`` () =
    let p = []
    let rt0 = { defaultRuntime with Queue = p }
    expectOk (runUntilHalt rt0) (fun rt ->        
        Assert.Empty(rt.Queue)
        Assert.Empty(rt.Stack))
    
[<Fact>]
let ``trace accumulation on success`` () =
    let p = [Int 1; Symbol "dup"]
    let rt0 = { defaultRuntime with Queue = p }
    // Execute using `step` as a control value.
    let rtA = rt0 |> step |> Result.bind step
    // Execute using runUntilHalt and compare with control value.
    let rtB = rt0 |> runUntilHalt
    match rtA, rtB with
    | Ok a, Ok b ->
        let va = Diagnostics.initRuntimeStateView a
        let vb = Diagnostics.initRuntimeStateView b
        // Make sure we have two traces (one for `1` and one for `dup`).        
        Assert.Equal(2, va.TraceSummary.Length)
        // Make sure that executing via `step` and `runUntilHalt` gives
        // the same result.       
        Assert.Equal(va, vb)
    | _ ->
        Assert.Fail("Runtimes should be identical")

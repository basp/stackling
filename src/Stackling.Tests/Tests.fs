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

let mkRuntime p =
    { defaultRuntime with Queue = p }

[<Fact>]
let ``int literals are pushed onto the stack`` () =
    let p = [Int 1]
    let rt0 = mkRuntime p
    let test rt =
        let trace = rt.Trace.Head
        Assert.Equal(Int 1, rt.Stack.Head)
        Assert.Empty(rt.Queue)
        Assert.Equal(None, trace.Resolution)        
    expectOk (step rt0) test
    
[<Fact>]
let ``dup should duplicate value on top of stack`` () =
    let p = [Int 1; Symbol "dup"]
    let rt0 = mkRuntime p
    let test rt =
        match rt.Stack with
        | _ :: _ :: _ ->
            Assert.StrictEqual([Int 1; Int 1], rt.Stack)
        | _ ->
            Assert.Fail("Stack is not of length 2")        
    // Execute using `step` as a sanity check (since most
    // tests use `runUntilHalt` instead).
    let rt1 = rt0 |> step |> Result.bind step
    test |> expectOk rt1

[<Fact>]
let ``literal only program`` () =
    let p = [Int 1; Int 2; Int 3]
    let rt0 = mkRuntime p
    let test rt =
        match rt.Stack with
        | _ :: _ ::_z :: _ ->
            Assert.StrictEqual([Int 3; Int 2; Int 1], rt.Stack)
        | _ ->
            Assert.Fail("Stack is not of length 3")
    test |> expectOk (runUntilHalt rt0)

[<Fact>]
let ``builtin only program`` () =
    let p = [Int 1; Symbol "dup"; Symbol "dup"]
    let rt0 = mkRuntime p
    let test rt =
        match rt.Stack with
        | _ :: _ :: _ :: _ ->
            Assert.StrictEqual([Int 1; Int 1; Int 1], rt.Stack)
        | _ ->
            Assert.Fail("Stack is not of length 3")        
    test |> expectOk (runUntilHalt rt0)
    
[<Fact>]
let ``user-defined word expansion`` () =
    let foo = Defined [Int 1; Int 2; Symbol "dup"]
    let env = defaultEnv |> Map.add "foo" foo
    let rt0 = { mkRuntime [Symbol "foo"] with Env = env }
    let test rt = 
         match rt.Stack with
         | _ :: _ :: _ :: _ ->
             Assert.StrictEqual([Int 2; Int 2; Int 1], rt.Stack)
         | _ ->
             Assert.Fail("Stack is not of length 3")
    test |> expectOk (runUntilHalt rt0)
    
[<Fact>]
let ``error propagation`` () =
    let p = [Symbol "dup"]
    let rt0 = mkRuntime p
    let test err traceEntry =
        Assert.Equal(Symbol "dup", traceEntry.Instruction)
        Assert.Equal(StackUnderflow, err)
    expectError
        (runUntilHalt rt0)
        (fun (err, traceEntry, _) -> test err traceEntry)
      
[<Fact>]
let ``instruction is consumed on error`` () =
    let p = [Symbol "dup"]
    let rt0 = mkRuntime p
    let test rt = Assert.Empty(rt.Queue)
    expectError
        (runUntilHalt rt0)
        (fun (_, _, rt) -> test rt)

[<Fact>]
let ``halt on empty queue`` () =
    let p = []
    let rt0 = mkRuntime p
    let test rt =
        Assert.Empty(rt.Queue)
        Assert.Empty(rt.Stack)        
    test |> expectOk (runUntilHalt rt0)
    
[<Fact>]
let ``trace accumulation on success`` () =
    let p = [Int 1; Symbol "dup"]
    let rt0 = mkRuntime p
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

[<Fact>]
let ``trace ordering and accumulation`` () =
    let p = [Int 1; Int 2; Symbol "dup"]
    let rt0 = mkRuntime p
    let test rt =
        let instructions = rt.Trace |> List.map (fun t -> t.Instruction)
        // Trace is stored in reverse order (last step first).
        let expected = [Symbol "dup"; Int 2; Int 1]
        Assert.StrictEqual(expected, instructions)        
    expectOk (runUntilHalt rt0) test
    
[<Fact>]
let ``trace entry correctness for builtins`` () =
    let p = [Int 1; Symbol "dup"]
    let rt0 = mkRuntime p
    
    let rt1 = rt0 |> step
    let test1 rt =
        Assert.Equal(Int 1, rt.Trace.Head.Instruction)
        Assert.StrictEqual([], rt.Trace.Head.StackBefore)
        Assert.StrictEqual([Int 1], rt.Trace.Head.StackAfter)
        Assert.StrictEqual([Int 1; Symbol "dup"], rt.Trace.Head.QueueBefore)
        Assert.StrictEqual([Symbol "dup"], rt.Trace.Head.QueueAfter)
    expectOk rt1 test1
    
    let rt2 = rt1 |> Result.bind step
    let test2 rt =
        Assert.Equal(Symbol "dup", rt.Trace.Head.Instruction)
        Assert.StrictEqual([Int 1], rt.Trace.Head.StackBefore)
        Assert.StrictEqual([Int 1; Int 1], rt.Trace.Head.StackAfter)
        Assert.StrictEqual([Symbol "dup"], rt.Trace.Head.QueueBefore)
        Assert.StrictEqual([], rt.Trace.Head.QueueAfter)
    expectOk rt2 test2

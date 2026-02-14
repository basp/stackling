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
            Assert.Equal<JoyValue list>([Int 1; Int 1], rt.Stack)
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
            Assert.Equal<JoyValue list>([Int 3; Int 2; Int 1], rt.Stack)
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
            Assert.Equal<JoyValue list>([Int 1; Int 1; Int 1], rt.Stack)
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
             Assert.Equal<JoyValue list>([Int 2; Int 2; Int 1], rt.Stack)
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
        Assert.Equal<JoyValue list>(expected, instructions)        
    expectOk (runUntilHalt rt0) test
    
[<Fact>]
let ``trace entry correctness for builtins`` () =
    let p = [Int 1; Symbol "dup"]
    let rt0 = mkRuntime p
    
    let rt1 = rt0 |> step
    let test1 rt =
        Assert.Equal(Int 1, rt.Trace.Head.Instruction)
        Assert.Equal<JoyValue list>([], rt.Trace.Head.StackBefore)
        Assert.Equal<JoyValue list>([Int 1], rt.Trace.Head.StackAfter)
        Assert.Equal<JoyValue list>([Int 1; Symbol "dup"], rt.Trace.Head.QueueBefore)
        Assert.Equal<JoyValue list>([Symbol "dup"], rt.Trace.Head.QueueAfter)
    expectOk rt1 test1
    
    let rt2 = rt1 |> Result.bind step
    let test2 rt =
        Assert.Equal(Symbol "dup", rt.Trace.Head.Instruction)
        Assert.Equal<JoyValue list>([Int 1], rt.Trace.Head.StackBefore)
        Assert.Equal<JoyValue list>([Int 1; Int 1], rt.Trace.Head.StackAfter)
        Assert.Equal<JoyValue list>([Symbol "dup"], rt.Trace.Head.QueueBefore)
        Assert.Equal<JoyValue list>([], rt.Trace.Head.QueueAfter)
    expectOk rt2 test2

[<Fact>]
let ``trace entry correctness for user-defined words`` () =
    let env =
        defaultEnv
        |> Map.add "foo" (Defined [Int 1; Int 2])
    let p = [Symbol "foo"]
    let rt0 = { mkRuntime p with Env = env }
    let rt1 = rt0 |> step
    let test rt = 
        Assert.True(rt.Trace.Head.Resolution.IsSome)
        Assert.Equal(Symbol "foo", rt.Trace.Head.Instruction)
        Assert.Equal<JoyValue list>([Symbol "foo"], rt.Trace.Head.QueueBefore)
        Assert.Equal<JoyValue list>([Int 1; Int 2], rt.Trace.Head.QueueAfter)
        Assert.Equal<JoyValue list>([], rt.Trace.Head.StackBefore)
        Assert.Equal<JoyValue list>([], rt.Trace.Head.StackAfter)
    expectOk rt1 test
    
[<Fact>]
let ``idempotence of halting`` () =
    // Run once until halting.
    let rt0 =
        runUntilHalt defaultRuntime
    // Run until halt and then re-use the same
    // runtime state to run again. The results of
    // running `rt0` and `rt1` should be identical.
    let rt1 =
        runUntilHalt defaultRuntime
        |> Result.bind runUntilHalt
    match rt0, rt1 with
    | Ok a, Ok b ->
        let va = Diagnostics.initRuntimeStateView a
        let vb = Diagnostics.initRuntimeStateView b
        Assert.Equal(va, vb)
    | _ ->
        Assert.Fail("Expected Ok")
    
[<Fact>]
let ``quote expansion chaining`` () =
    let foo = [Int 1; Symbol "bar"]
    let bar = [Int 2; Symbol "dup"]
    let env =
        defaultEnv
        |> Map.add "bar" (Defined bar)
        |> Map.add "foo" (Defined foo)    
    let rt0 = { mkRuntime [Symbol "foo"] with Env = env }
    let rt1 = runUntilHalt rt0
    let test rt =
        Assert.Equal<JoyValue list>([Int 2; Int 2; Int 1], rt.Stack)
    expectOk rt1 test
    
[<Fact>]
let ``swap stack discipline invariants`` () =
    let p0 = [Int 1; Int 2; Symbol "swap"]
    let p1 = [Int 1; Symbol "swap"]
    
    let testOk rt =
        Assert.Equal<JoyValue list>([Int 1; Int 2], rt.Stack)
    let testError (err, traceEntry, rt) =
        Assert.Equal(StackUnderflow, err)
        Assert.Equal(Symbol "swap", traceEntry.Instruction)
        Assert.Equal<JoyValue list>([Int 1], rt.Stack)
        
    mkRuntime p0
    |> runUntilHalt
    |> (fun x -> expectOk x testOk)

    mkRuntime p1
    |> runUntilHalt
    |> (fun x -> expectError x testError)
    
[<Fact>]
let ``the i combinator`` () =
    let p = [Quotation [Int 1; Int 2; Symbol "swap"]; Symbol "i"]
    let rt0 = mkRuntime p
    expectOk (runUntilHalt rt0) (fun rt ->
        Assert.Equal<JoyValue list>([Int 1; Int 2], rt.Stack))
    
[<Fact>]
let ``nested quotations`` () =
    let p = [
        Quotation [
            Quotation [Int 1; Int 2; Symbol "swap"]
            Symbol "i" ]
        Symbol "i" ]    
    let rt0 = mkRuntime p
    expectOk (runUntilHalt rt0) (fun rt ->
        Assert.Equal<JoyValue list>([Int 1; Int 2], rt.Stack))
    
[<Fact>]
let ``i does not evaluate non-quotation values`` () =
    let p = [Int 1; Symbol "i"]
    let rt0 = mkRuntime p
    let testError (err, _, _) =
        match err with
        | InvalidQuotation _ -> Assert.True(true);
        | _ -> Assert.Fail("Expected InvalidQuotation")
    expectError (runUntilHalt rt0) testError

[<Fact>]
let ``i inside a user-defined word`` () =
    let foo = Defined [Quotation [Int 1; Int 2; Symbol "swap"]; Symbol "i"]
    let env = defaultEnv |> Map.add "foo" foo
    let rt0 = { mkRuntime [Symbol "foo"] with Env = env }
    expectOk (runUntilHalt rt0) (fun rt ->
        Assert.Equal<JoyValue list>([Int 1; Int 2], rt.Stack))
    
[<Fact>]
let ``triple-nested quotations`` () =
    // [ [ [1 2 swap] i ] i swap ] i swap
    //                             ^
    // [ [1 2 swap] i ] i swap swap
    //                  ^
    // [1 2 swap] i swap swap
    //            ^
    // 1 2 swap swap swap
    //
    // => [1; 2]
    let p = [
        Quotation [
            Quotation [
                Quotation [Int 1; Int 2; Symbol "swap"]
                Symbol "i"
            ]
            Symbol "i"
            Symbol "swap"
        ]
        Symbol "i"
        Symbol "swap" ]
    let rt0 = mkRuntime p
    expectOk (runUntilHalt rt0) (fun rt ->
        Assert.Equal<JoyValue list>([Int 1; Int 2], rt.Stack))
    
[<Fact>]
let ``i applied to empty quotation`` () =
    let p = [Quotation []; Symbol "i"]
    let rt0 = mkRuntime p
    expectOk (runUntilHalt rt0) (fun rt -> Assert.Empty(rt.Stack))
    
[<Fact>]
let ``i inside a quotation inside a user-defined word`` () =
    let foo = [
        Quotation [
            Quotation [
                Quotation [Int 1; Int 2; Symbol "swap"]
                Symbol "i"
            ]
            Symbol "i"
            Symbol "swap"
        ]
        Symbol "i"
        Symbol "swap" ]
    let p = [Quotation [Symbol "foo"]; Symbol "i"]
    let env = defaultEnv |> Map.add "foo" (Defined foo)
    let rt0 = { mkRuntime p with Env = env }
    expectOk (runUntilHalt rt0) (fun rt ->
        Assert.Equal<JoyValue list>([Int 1; Int 2], rt.Stack))
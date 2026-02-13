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
    let r0 = { defaultRuntime with Queue = [Int 1; Symbol "dup"] }
    let r1 = r0 |> step |> Result.bind step
    expectOk r1 (fun rt ->
        match rt.Stack with
        | x :: y :: _ ->
            Assert.Equal(Int 1, x)
            Assert.Equal(Int 1, y)
        | _ -> Assert.Fail("Stack is not of length 2"))
      
[<Fact>]
let ``literal only program`` () =
    let p = [Int 1; Int 2; Int 3]
    let rt = { defaultRuntime with Queue = p }
    expectOk (runUntilHalt rt) (fun rt ->
        match rt.Stack with
        | x :: y :: z :: _ ->
            Assert.Equal(Int 3, x)
            Assert.Equal(Int 2, y)
            Assert.Equal(Int 1, z)
        | _ -> Assert.Fail("Stack is not of length 3"))

[<Fact>]
let ``builtin only program`` () =
    let p = [Int 1; Symbol "dup"; Symbol "dup"]
    let rt = { defaultRuntime with Queue = p }
    expectOk (runUntilHalt rt) (fun rt ->
        match rt.Stack with
        | x :: y :: z :: _ ->
            Assert.Equal(Int 1, x)
            Assert.Equal(Int 1, y)
            Assert.Equal(Int 1, z)
        | _ -> Assert.Fail("Stack is not of length 3"))
    
[<Fact>]
let ``user-defined word expansion`` () =
    let foo = Defined [Int 1; Int 2; Symbol "dup"]
    let env = defaultEnv |> Map.add "foo" foo
    let rt = { defaultRuntime with Env = env; Queue = [Symbol "foo"] }
    expectOk (runUntilHalt rt) (fun rt ->
         match rt.Stack with
         | x :: y :: z :: _ ->
             Assert.Equal(Int 2, x)
             Assert.Equal(Int 2, y)
             Assert.Equal(Int 1, z)
         | _ -> Assert.Fail("Stack is not of length 3"))
    
[<Fact>]
let ``error propagation`` () =
    let p = [Symbol "dup"]
    let rt = { defaultRuntime with Queue = p }
    expectError (runUntilHalt rt) (fun (err, traceEntry, _) ->
        Assert.Equal(Symbol "dup", traceEntry.Instruction)
        Assert.Equal(StackUnderflow, err))
      
[<Fact>]
let ``instruction is consumed on error`` () =
    let p = [Symbol "dup"]
    let tr = { defaultRuntime with Queue = p }
    expectError (runUntilHalt tr) (fun (_, _, rt) ->
        Assert.Empty(rt.Queue))

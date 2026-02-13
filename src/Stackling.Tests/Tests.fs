module Stackling.Tests

open Stackling.Runtime
open Stackling.Interpreter
open Xunit

[<Fact>]
let ``int literals are pushed onto the stack`` () =
    let rt0 = { defaultRuntime with Queue = [Int 1] }
    match step rt0 with
    | Ok rt1 ->
        let trace = rt1.Trace.Head
        Assert.Equal(Int 1, rt1.Stack.Head)
        Assert.Empty(rt1.Queue)
        Assert.Equal(None, trace.Resolution)
    | Error x ->
        Assert.Fail(x.ToString())

[<Fact>]
let ``dup should duplicate value on top of stack`` () =
    let r0 = { defaultRuntime with Queue = [Int 1; Symbol "dup"] }
    let r1 = r0 |> step |> Result.bind step
    match r1 with
    | Ok rt ->
        match rt.Stack with
        | x :: y :: _ ->
            Assert.Equal(Int 1, x)
            Assert.Equal(Int 1, y)        
        | _ ->
            Assert.Fail("Stack is not of length 2")
    | Error (err, _, _) ->
        Assert.Fail(err.ToString())
      
[<Fact>]
let ``literal only program`` () =
    let p = [Int 1; Int 2; Int 3]
    let rt = { defaultRuntime with Queue = p }
    match runUntilHalt rt with
    | Ok rt ->
        match rt.Stack with
        | x :: y :: z :: _ ->
            Assert.Equal(Int 3, x)
            Assert.Equal(Int 2, y)
            Assert.Equal(Int 1, z)
        | _ ->
            Assert.Fail("Stack is not of length 3")
    | err ->
        Assert.Fail(err.ToString())

[<Fact>]
let ``builtin only program`` () =
    let p = [Int 1; Symbol "dup"; Symbol "dup"]
    let rt = { defaultRuntime with Queue = p }
    match runUntilHalt rt with
    | Ok rt ->
        match rt.Stack with
        | x :: y :: z :: _ ->
            Assert.Equal(Int 1, x)
            Assert.Equal(Int 1, y)
            Assert.Equal(Int 1, z)
        | _ ->
            Assert.Fail("Stack is not of length 3")
    | err ->
        Assert.Fail(err.ToString())
    
[<Fact>]
let ``user-defined word expansion`` () =
    let foo = Defined [Int 1; Int 2; Symbol "dup"]
    let env = defaultEnv |> Map.add "foo" foo
    let rt = { defaultRuntime with Env = env; Queue = [Symbol "foo"] }
    match runUntilHalt rt with
    | Ok rt ->
        match rt.Stack with
        | x :: y :: z :: _ ->
            Assert.Equal(Int 2, x)
            Assert.Equal(Int 2, y)
            Assert.Equal(Int 1, z)
        | _ ->
            Assert.Fail("Stack is not of length 3")
    | err ->
        Assert.Fail(err.ToString())
    
[<Fact>]
let ``error propagation`` () =
    let p = [Symbol "dup"]
    let rt = { defaultRuntime with Queue = p }
    match runUntilHalt rt with
    | Error (err, traceEntry, _) ->
        Assert.Equal(Symbol "dup", traceEntry.Instruction)
        Assert.Equal(err, StackUnderflow)
    | _ ->
        Assert.Fail("Expected error")
      
[<Fact>]
let ``instruction is consumed on error`` () =
    let p = [Symbol "dup"]
    let tr = { defaultRuntime with Queue = p }
    match runUntilHalt tr with
    | Error (_, _, rt) ->
        Assert.Empty(rt.Queue)
    | _ ->
        Assert.Fail("Expected error")

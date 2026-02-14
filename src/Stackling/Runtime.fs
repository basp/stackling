module Stackling.Runtime

type JoyValue =
    | Bool of bool
    | Int of int
    | Float of float
    | String of string
    | Symbol of string
    | Quotation of JoyValue list
    
    override x.ToString() =
        match x with
        | Bool b -> b.ToString()
        | Int i -> i.ToString()
        | Float f -> f.ToString()
        | String s -> s
        | Symbol name -> name
        | Quotation q ->
            (String.concat " " (List.map string q))
            |> sprintf "[%s]" 

and JoyDefinition =
    | Builtin of Builtin
    | Defined of JoyValue list
    
and Builtin =
    | Dup
    | Swap
    | Pop
    | I
    | Add
    | Sub
    | Div
    | Mul

and JoyError =
    | StackUnderflow
    | TypeMismatch of expected: string * actual: JoyValue
    | UndefinedSymbol of string
    | InvalidQuotation of JoyValue
    | DivisionByZero
    | TypeError of string
    
    override x.ToString() =
        match x with
        | StackUnderflow ->
            "Stack underflow"
        | TypeMismatch (expected, actual) ->
            $"Type mismatch: expected %s{expected}, got {actual}"
        | UndefinedSymbol name ->
            $"Undefined symbol: %s{name}"
        | InvalidQuotation q ->
            $"Invalid quotation: %A{q}"
        | DivisionByZero ->
            "Division by zero"
        | TypeError msg ->
            msg            

and Runtime = {
    Queue: JoyValue list
    Stack: JoyValue list
    Env: Map<string, JoyDefinition>
    Trace: TraceEntry list }

and TraceEntry = {
    Instruction: JoyValue
    StackBefore: JoyValue list
    StackAfter: JoyValue list
    QueueBefore: JoyValue list
    QueueAfter: JoyValue list
    Resolution: JoyDefinition option }

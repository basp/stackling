module Stackling.Runtime

type JoyValue =
    | Bool of bool
    | Int of int
    | String of string
    | Symbol of string
    | Quotation of JoyValue list
    
    override x.ToString() =
        match x with
        | Bool b -> b.ToString()
        | Int i -> i.ToString()
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

and JoyError =
    | StackUnderflow
    | TypeMismatch of expected: string * actual: JoyValue
    | UndefinedSymbol of string
    | InvalidQuotation of JoyValue
    
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

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

type JoyType =
    | TBool
    | TInt
    | TFloat
    | TString
    | TSymbol
    | TQuotation
    | TNumeric
    | TAny

type StackEffect = JoyType list * JoyType list
    
type BuiltinInfo = {
    Impl: Runtime -> Result<Runtime, JoyError>
    Effect: StackEffect
}
    
// Renamed `valueType` to `joyTypeOf` to mimic similar F# apis.
let joyTypeOf =
    function
    | Bool _ -> TBool
    | Int _ -> TInt
    | Float _ -> TFloat
    | String _ -> TString
    | Symbol _ -> TSymbol
    | Quotation _ -> TQuotation    
    
// Reversed arguments to match xUnit convention and allow
// for convenient forward pipe usage.
let typeMatches expected actual =
    match expected with
    | TAny -> true
    | TNumeric ->
        match actual with
        | TInt -> true
        | TFloat -> true
        | _ -> false
    | _ -> actual = expected

let checkStackEffect (rt: Runtime) (effect: StackEffect) =
    // We ignore the outputs for now.
    let inputs, _ = effect
    let rec loop stack types =
        match stack, types with
        | _, [] -> Ok ()
        | actual :: actualRest, expected :: expectedRest ->
            match typeMatches expected actual with
            | true ->
                loop actualRest expectedRest
            | false ->
                Error (TypeError $"Expected %A{expected}, got %A{actual}")
        | [], _ ->
            Error StackUnderflow
    let stackTypes = rt.Stack |> List.map joyTypeOf
    loop stackTypes inputs
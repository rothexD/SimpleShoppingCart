module Parser

open System

let safeEquals (it : string) (theOther : string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let (|AddToCart|RemoveFromCart|SetQuantityInCart|Undo|ListStoreItems|Help|ParseFailed|) (input : string) =
    let tryParseInt (arg : string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg
        if worked then valueConstructor arg' else ParseFailed

    let tryParseInt2 (arg1 : string) (arg2: string) valueConstructor =
        let (worked1, arg1') = Int32.TryParse arg1
        let (worked2, arg2') = Int32.TryParse arg2
        if worked1 && worked2 then valueConstructor arg1' arg2' else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with
    | [ verb; arg ] when safeEquals verb (nameof MessageTypes.Add) ->
        tryParseInt arg (fun value -> AddToCart value)
    | [ verb; arg ] when safeEquals verb (nameof MessageTypes.Remove) ->
        tryParseInt arg (fun value -> RemoveFromCart value)
    | [ verb; arg; arg2 ] when safeEquals verb (nameof MessageTypes.SetQuantity) ->
        tryParseInt2 arg arg2 (fun value1 value2 -> SetQuantityInCart (value1 value2))
    | [ verb ] when safeEquals verb (nameof MessageTypes.Undo) -> Undo
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | [ verb; arg ] when safeEquals verb (nameof Domain.IncrementBy) ->
        tryParseInt arg (fun value -> IncrementBy value)
    | [ verb; arg ] when safeEquals verb (nameof Domain.DecrementBy) ->
        tryParseInt arg (fun value -> DecrementBy value)
    | _ -> ParseFailed

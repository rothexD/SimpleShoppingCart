module Parser

open System

let safeEquals (it : string) (theOther : string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let rec (|AddToCart|RemoveFromCart|SetQuantityInCart|Undo|ListStoreItems|Help|ParseFailed|) (input : string) =
    let tryParseInt (arg : string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg
        if worked then valueConstructor arg' else ParseFailed

    let tryParseInt2 (arg1 : string) (arg2: string) valueConstructor =
        let (worked1, arg1') = Int32.TryParse arg1
        let (worked2, arg2') = Int32.TryParse arg2
        if worked1 && worked2 then valueConstructor (arg1',arg2') else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with
    | [ verb; arg; arg2 ] when safeEquals verb (nameof MessageTypes.Add) ->
        tryParseInt2 arg arg2 (fun value-> AddToCart value)
    | [ verb; arg ] when safeEquals verb (nameof MessageTypes.Remove) ->
        tryParseInt arg (fun value -> RemoveFromCart value)
    | [ verb; arg; arg2 ] when safeEquals verb (nameof MessageTypes.SetQuantity) ->
        tryParseInt2 arg arg2 (fun value-> SetQuantityInCart value)
    | [ verb; arg ] when safeEquals verb (nameof MessageTypes.Undo) ->
        tryParseInt arg (fun value -> Undo value)
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | [ verb ] when safeEquals verb (nameof MessageTypes.PrintStoreItems) -> ListStoreItems
    | _ -> ParseFailed

let rec (|Checkout|EnterPersonalDetails|SelectPaymentMethod|Pay|ParseFailed|) (input : string) =
    let tryParseInt (arg : string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg
        if worked then valueConstructor arg' else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with
    | [ verb; ] when safeEquals verb (nameof MessageTypes.Checkout) -> Checkout
    | [ verb; arg1; arg2; arg3 ] when safeEquals verb (nameof MessageTypes.EnterPersonalDetails) -> EnterPersonalDetails (arg1, arg2, arg3)
    | [ verb; arg; ] when safeEquals verb (nameof MessageTypes.SetQuantity) ->
        tryParseInt arg (fun value-> SelectPaymentMethod value)
    | [ verb; arg1; arg2 ] when safeEquals verb (nameof MessageTypes.Pay) -> Pay (arg1, arg2)
    | _ -> ParseFailed

module Repl

open System
open DomainModels
open Parser

type Message =
    | DomainMessage of MessageTypes.Message
    | HelpRequested
    | NotParsable of string

let read (input : string) =
    match input with
    | AddToCart v -> MessageTypes.Add v |> DomainMessage
    | RemoveFromCart v -> MessageTypes.Remove v |> DomainMessage
    | SetQuantityInCart v -> MessageTypes.SetQuantity v  |> DomainMessage
    | Undo v -> MessageTypes.Undo v |> DomainMessage
    | ListStoreItems v -> MessageTypes.PrintStoreItems |> DomainMessage
    | Help -> HelpRequested
    | ParseFailed  -> NotParsable input

open Microsoft.FSharp.Reflection

let createHelpText () : string =
    FSharpType.GetUnionCases typeof<MessageTypes.Message >
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")


let itemStateToString (state: ItemState) (index: int): string =
    $"{index}\t{state.Item.Name}\t\t\t{state.Item.Price}$\t({state.Quantity}){Environment.NewLine}"

let cartToString (cart: Cart): string =
    let state = cart.States.Peek()
    let headerLines = $"Index\tName\t\t\tPrice\tQuantity{Environment.NewLine}";
    let strings = state.Items |> Map.toList |> List.mapi (fun index (_, itemState) -> itemStateToString itemState index)
    let sumString = [$"Total: {state.Sum}$"]
    String.Join(Environment.NewLine, (headerLines :: strings) @ sumString)

let evaluate (update : MessageTypes.Message -> Cart -> Cart) (cart : Cart) (msg : Message) =
    match msg with
    | DomainMessage msg ->
        let newCart = update msg cart
        let textToPrint = cartToString newCart
        let message = $"{Environment.NewLine}{Environment.NewLine}The message was {msg}.{Environment.NewLine}Cart content: {Environment.NewLine}{textToPrint}"
        (newCart, message)
    | HelpRequested ->
        let message = createHelpText ()
        (cart, message)
    | NotParsable originalInput ->
        let message =
            sprintf """"%s" was not parsable. %s"""  originalInput "You can get information about known commands by typing \"Help\""
        (cart, message)

let print (state : Cart, outputToPrint : string) =
    printfn "%s\n" outputToPrint
    printf "> "

    state

let rec loop (state : Cart) =
    Console.ReadLine()
    |> read
    |> evaluate Domain.update state
    |> print
    |> loop

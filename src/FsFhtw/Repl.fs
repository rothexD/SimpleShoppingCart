module Repl

open System
open DomainModels
open Parser

type Message =
    | DomainMessage of MessageTypes.Message
    | HelpRequested
    | NotParsable of string

type State = DomainModels.Cart

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

let evaluate (update : MessageTypes.Message -> State -> State) (state : State) (msg : Message) =
    match msg with
    | DomainMessage msg ->
        let newState = update msg state
        let message = sprintf "The message was %A. New state is %A" msg newState
        (newState, message)
    | HelpRequested ->
        let message = createHelpText ()
        (state, message)
    | NotParsable originalInput ->
        let message =
            sprintf """"%s" was not parsable. %s"""  originalInput "You can get information about known commands by typing \"Help\""
        (state, message)

let print (state : State, outputToPrint : string) =
    printfn "%s\n" outputToPrint
    printf "> "

    state

let rec loop (state : Cart) =
    Console.ReadLine()
    |> read
    |> evaluate Domain.update state
    |> print
    |> loop

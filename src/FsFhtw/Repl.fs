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
    | Checkout v -> MessageTypes.Checkout |> DomainMessage
    | EnterPersonalDetails v -> MessageTypes.EnterPersonalDetails v |> DomainMessage
    | SelectPaymentMethod v -> MessageTypes.SelectPaymentMethod v |> DomainMessage
    | Pay v -> MessageTypes.Pay v |> DomainMessage
    | Help -> HelpRequested
    | ParseFailed  -> NotParsable input
    | PrintCart v -> MessageTypes.PrintCart |> DomainMessage


open Microsoft.FSharp.Reflection

let createHelpText (cart: Cart) : string =
    let header = FSharpType.GetUnionCases typeof<MessageTypes.Message >
                 |> Array.map (fun case -> case.Name)
                 |> Array.fold (fun prev curr -> prev + " " + curr) ""
                 |> (fun s -> s.Trim() |> sprintf "Known commands are: %s" )
    let context = "the following commands are currently allowed"
    let body = if not cart.CheckoutInProgress && cart.States.Peek().Sum = decimal 0
                    then "\tAdd <Item-From-Store-Number> <Quantity>" + Environment.NewLine +
                         "\tRemove <Item-In-Cart-Number>" +  Environment.NewLine +
                         "\tSetQuantity <Item-In-Cart-Number>" + Environment.NewLine +
                         "\tUndo <Number-of-Operations-To-Undo>" + Environment.NewLine +
                         "\tPrintCart"+ Environment.NewLine +
                         "\tPrintStoreItems"
                else if not cart.CheckoutInProgress
                    then "\tAdd <Item-From-Store-Number> <Quantity>" + Environment.NewLine +
                         "\tRemove <Item-In-Cart-Number>" +  Environment.NewLine +
                         "\tSetQuantity <Item-In-Cart-Number>" + Environment.NewLine +
                         "\tUndo <Number-of-Operations-To-Undo>" + Environment.NewLine +
                         "\tPrintStoreItems" + Environment.NewLine +
                         "\tPrintCart"+ Environment.NewLine +
                         "\tCheckout"
                else if cart.UserData.IsNone
                    then "\tEnterPersonalDetails <Name> <Address> <Email>" + Environment.NewLine +
                         "\tPrintCart"
                else if cart.SelectedPaymentMethod.IsNone
                    then "\tSelectPaymentMethod <PaymentMethod-Number>" + Environment.NewLine +
                         "\t\t(0) PayPal" + Environment.NewLine +
                         "\t\t(1) CreditCard" + Environment.NewLine +
                         "\tPrintCart"
                else if cart.SelectedPaymentMethod.Value.ToLower() = "PayPal"
                    then "\tpay <username> <password>" + Environment.NewLine +
                         "\tPrintCart"
                else "\tpay <credit-card-number> <cvv>" + Environment.NewLine +
                         "\tPrintCart"
    header + Environment.NewLine + context + Environment.NewLine+ body

let evaluate (update : MessageTypes.Message -> Cart -> Cart) (cart : Cart) (msg : Message) =
    match msg with
    | DomainMessage msg ->
        let newCart = update msg cart
        let message = $"{Environment.NewLine}{Environment.NewLine}The message was {msg}"
        (newCart, message)
    | HelpRequested ->
        let message = createHelpText (cart)
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

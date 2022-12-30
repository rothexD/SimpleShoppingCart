module Domain

open System
open System.Collections.Generic
open Microsoft.FSharp.Core

// Domain Model
type Item = { Id: Guid; Name: string; Price: decimal }
type ItemState = { Item: Item; Quantity: int }

type State() =
    member val Items = Map<Guid, ItemState>([])
    member val Sum: decimal = decimal 0 with get, set

    member this.Add(item: Item, quantity: int): State =
        //let savedQuantity = 0
        //if this.Items.ContainsKey item.Id
        //then savedQuantity = this.Items[item.Id].Quantity + quantity
        //else savedQuantity = quantity
        failwith ":(";

    member this.getDeepCopy(): State =
        failwith "NotImplementedException";


type Cart() =
    let CurrentState: State = State();
    let PastStates: Stack<State> = Stack<State> []

    let saveState() =
        let deepCopy = CurrentState.getDeepCopy()
        PastStates.Push(deepCopy);

    let Add(item: Item, quantity: int) =
        saveState();
        CurrentState.Add item quantity

    let Undo() =
        let x = PastStates.Pop();

type Store = { Items: list<Item> }

// Message types
type Add = { Item: Item; Quantity: int }
type Remove = { Item: Item; }
type SetQuantity = { Item: Item; Quantity: int }

type Message =
    | Add of Add
    | Remove of Remove
    | SetQuantity of SetQuantity
    | Undo of int


let update (msg : Message) (cart : Cart) : State =
    match msg with
    | Add -> model + 1
    | Remove -> model - 1
    | SetQuantity x -> model + x
    | Undo x -> model - x

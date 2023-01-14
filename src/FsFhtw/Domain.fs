module Domain

open System
open System.Collections.Generic
open DomainModels
open MessageTypes
open Microsoft.FSharp.Core


// State functions
let addToState (state: State) (item: Item) (quantity: decimal): State =
    {
        Items = state.Items |> Map.change item.Id (fun x ->
            match x with
            | Some itemState -> Some { Item = item; Quantity = itemState.Quantity + quantity }
            | None -> Some { Item = item; Quantity = quantity }
        )
        Sum = state.Sum + item.Price * quantity;
    };

let removeFromState (state : State) (item : Item) : State =
    let oldSum = state.Sum
    let oldValues = state.Items.TryFind(item.Id)
    match oldValues with
    | None -> state
    | Some x ->
        let newSum = oldSum - (x.Quantity * x.Item.Price)
        { Items = state.Items.Remove(item.Id); Sum = newSum}

let setQuantityInState (state : State) (item: Item) (itemQuantity: decimal): State =
    if (itemQuantity <= decimal 0)
    then removeFromState state item
    else
        let oldItemPrice = state.Items[item.Id].Quantity * state.Items[item.Id].Item.Price
        let newItemPrice = item.Price * itemQuantity;
        {
            Items = state.Items |> Map.change item.Id (fun x ->
                match x with
                    | Some _ -> Some { Item = item; Quantity =  itemQuantity }
                    | None -> None)
            Sum = state.Sum - oldItemPrice + newItemPrice;
        }


// Cart functions
let updateCartState (cart: Cart) (update: State -> State): Cart =
    let currentState = cart.States.Peek()
    let newState = update currentState
    cart.States.Push newState
    cart

let addToCart (cart: Cart) (item: Option<Item>) (quantity: decimal): Cart =
    match item with
    |Some x -> updateCartState cart (fun state -> addToState state x quantity)
    |None -> cart

let removeFromCart (cart: Cart) (item: Option<Item>): Cart =
    match item with
    |Some x ->  updateCartState cart (fun state -> removeFromState state x)
    |None -> cart

let setQuantityInCart (cart: Cart) (item: Option<Item>) (quantity: decimal): Cart =
    match item with
    |Some x -> updateCartState cart (fun state -> setQuantityInState state x quantity)
    |None -> cart

let rec undoCartActions (cart: Cart) (steps: decimal): Cart =
    if cart.States.Count > 1 && steps > decimal 0
        then
            let _ = cart.States.Pop();
            undoCartActions cart (steps - decimal 1)
        else cart

let printStoreItem (index: int) (item: Item) =
    printf $"{index}\t {item.Name}\t {item.Price}$ {Environment.NewLine}"

let printItemsInStore(store: Store)(cart: Cart): Cart =
    printf $"Index\t Name\t Price {Environment.NewLine}";
    store.Items |> List.iteri printStoreItem
    cart

let initCart(): Cart =
    {
        States = Stack<State> [{
            Items = Map<Guid, ItemState>([])
            Sum = decimal 0
        }]
    }


// Payment functions
let checkout (cart: Cart): Cart =
    failwith ""

let enterPersonalDetails (cart: Cart) (name: string) (address: string) (email: string): Cart =
    failwith ""

let selectPaymentMethod (cart: Cart) (index: int) =
    failwith ""

// credential A is either username or credit card number
// credential B is either password or CVV
let pay (cart: Cart) (credentialA: string) (credentialB: string): Cart =
    failwith ""

// Store functions
let store = {Items = LoadItemsFromFile.loadItemsFromFile "ItemsInStore.txt"}

let GetItemByIndexerFromStore(index: int) : Option<Item> =
    if store.Items.Length < index || index < 0
    then
       None
    else
       Some store.Items[index]
let GetItemByIndexerFromCart (cart:Cart) (index: int) : Option<Item> =
    if cart.States.Peek().Items.Count <= index || index < 0
    then
       None
    else
        Some (List.ofSeq(cart.States.Peek().Items)[index]).Value.Item


// Message processing functions
let update (msg : Message) (cart : Cart) : Cart =
    match msg with
    | Add (x,y) -> addToCart cart (GetItemByIndexerFromStore x) (decimal y)
    | Remove x -> removeFromCart cart (GetItemByIndexerFromCart cart x)
    | SetQuantity (x,y) -> setQuantityInCart cart (GetItemByIndexerFromCart cart x) (decimal y)
    | Undo steps -> undoCartActions cart (decimal steps)
    | PrintStoreItems -> printItemsInStore store cart
    | Checkout -> checkout cart
    | EnterPersonalDetails (name, address, email) -> enterPersonalDetails cart name address email
    | SelectPaymentMethod index -> selectPaymentMethod cart index
    | Pay (credentialA, credentialB) -> pay cart credentialA credentialB

type Message =
    | Add of int * int
    | Remove of int
    | SetQuantity of int * int
    | Undo of int
    | PrintStoreItems
    | Checkout
    | EnterPersonalDetails of string * string * string
    | SelectPaymentMethod of int
    | Pay of string * string


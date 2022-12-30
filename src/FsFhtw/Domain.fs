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
    if state.Items.ContainsKey item.Id
        then
            let oldPriceForItemAndQuantity = state.Items[item.Id].Quantity * state.Items[item.Id].Item.Price
            {
                Items = state.Items |> Map.change item.Id (fun x ->
                match x with
                    | Some itemState -> Some { Item = item; Quantity =  itemQuantity }
                    | None -> None)
                Sum = state.Sum - oldPriceForItemAndQuantity + item.Price * itemQuantity;
            }
    else
       if itemQuantity > decimal 0
            then
                addToState state item itemQuantity
            else
                state


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
        then undoCartActions cart (steps - decimal 1)
        else cart

let printStoreItem (index: int) (item: Item) =
    printf $"{index}\t {item.Name}\t {item.Price} {Environment.NewLine}"

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

// Store functions
let store = {Items = LoadItemsFromFile.loadItemsFromFile "ItemsInStore.txt"}

let GetItemByIndexerFromStore(index: int) : Option<Item> =
    if store.Items.Length < index || index < 0
    then
       None
    else
       Some store.Items[index]
let GetItemByIndexerFromCart (cart:Cart) (index: int) : Option<Item> =
    if cart.States.Pop().Items.Count < index || index < 0
    then
       None
    else
        Some (List.ofSeq(cart.States.Pop().Items)[index]).Value.Item

// Message processing functions
let update (msg : Message) (cart : Cart) : Cart =
    match msg with
    | Add (x,y) -> addToCart cart (GetItemByIndexerFromStore x) (decimal y)
    | Remove x -> removeFromCart cart (GetItemByIndexerFromCart cart x)
    | SetQuantity (x,y) -> setQuantityInCart cart (GetItemByIndexerFromCart cart x) (decimal y)
    | Undo steps -> undoCartActions cart (decimal steps)
    | PrintStoreItems -> printItemsInStore store cart

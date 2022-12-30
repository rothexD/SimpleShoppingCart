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

let addToCart (cart: Cart) (item: Item) (quantity: decimal): Cart =
    updateCartState cart (fun state -> addToState state item quantity)

let removeFromCart (cart: Cart) (item: Item): Cart =
    updateCartState cart (fun state -> removeFromState state item)

let setQuantityInCart (cart: Cart) (item: Item) (quantity: decimal): Cart =
    updateCartState cart (fun state -> setQuantityInState state item quantity)

let rec undoCartActions (cart: Cart) (steps: decimal): Cart =
    if cart.States.Count > 1 && steps > decimal 0
        then undoCartActions cart (steps - decimal 1)
        else cart

let printStoreItem (index: int) (item: Item) =
    printf $"{index}\t {item.Name}\t {item.Price}"

let printItemsInStore(store: Store, cart: Cart): Cart =
    printf "Index\t Name\t Price";
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
let store = LoadItemsFromFile.loadItemsFromFile "ItemsInStore.txt"




// Message processing functions
let update (msg : Message) (cart : Cart) : Cart =
    match msg with
    | Add x -> addToCart cart x.Item x.Quantity
    | Remove x -> removeFromCart cart x.Item
    | SetQuantity x -> setQuantityInCart cart x.Item x.Quantity
    | Undo steps -> undoCartActions cart steps
    | PrintStoreItems -> printItemsInStore store cart

module Domain

open System
open System.Collections.Generic
open System.IO
open System.Reflection.Metadata
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
        CheckoutInProgress = false
        SelectedPaymentMethod = None;
        UserData = None
        Credentials = None;
    }

// Funny string stuff
let itemStateToString (state: ItemState) (index: int): string =
    $"{index}\t{state.Item.Name}\t\t\t{state.Item.Price}$\t({state.Quantity}){Environment.NewLine}"

let cartToString (cart: Cart): string =
    let state = cart.States.Peek()
    let headerLines = $"Index\tName\t\t\tPrice\tQuantity{Environment.NewLine}";
    let strings = state.Items |> Map.toList |> List.mapi (fun index (_, itemState) -> itemStateToString itemState index)
    let sumString = [$"Total: {state.Sum}$"]
    String.Join(Environment.NewLine, (headerLines :: strings) @ sumString)

// Payment functions
let checkout (cart: Cart): Cart =
    {
        States = cart.States;
        CheckoutInProgress = true;
        UserData = None;
        Credentials = None;
        SelectedPaymentMethod = None;
    }

let enterPersonalDetails (cart: Cart) (name: string) (address: string) (email: string): Cart =
    let userData = {Name=name;Address=address;Email=email}
    {
        States = cart.States
        CheckoutInProgress = cart.CheckoutInProgress
        SelectedPaymentMethod = cart.SelectedPaymentMethod
        UserData = Some(userData)
        Credentials = cart.Credentials
    }

let selectPaymentMethod (cart: Cart) (index: int): Cart =
    {
        States = cart.States;
        CheckoutInProgress = cart.CheckoutInProgress;
        UserData = cart.UserData;
        Credentials = cart.Credentials;
        SelectedPaymentMethod = Some (if index = 0 then "PayPal" else "Credit Card");
    }

let rec saveReceipt (cart: Cart): Guid =
    let cartState = cart.States.Peek()
    let shoppingId = Guid.NewGuid()
    let shoppingIdString = $"{shoppingId}";
    let fileName = $"receipts/receipt-{shoppingId}.txt"

    let itemList = cartState.Items |> Map.map (fun _ itemState ->
        $"{itemState.Item.Name} ({itemState.Quantity}): {itemState.Item.Price * itemState.Quantity}")

    let receipt = shoppingIdString + Environment.NewLine
                  + "Name: " + cart.UserData.Value.Name + Environment.NewLine
                  + "Address: " + cart.UserData.Value.Address + Environment.NewLine
                  + "Email: " + cart.UserData.Value.Email + Environment.NewLine
                  + "Items: " + Environment.NewLine
                  + String.Join (Environment.NewLine, itemList.Values) + Environment.NewLine
                  + $"Total: {cartState.Sum}"

    File.WriteAllText (fileName, receipt)
    shoppingId;

// credential A is either username or credit card number
// credential B is either password or CVV
let pay (cart: Cart) (credentialA: string) (credentialB: string): Cart =
    let shoppingId = saveReceipt cart
    printf $"Saved receipt, shopping id is {shoppingId}{Environment.NewLine}"
    let cartString = cartToString(cart)
    printf $"{cartString}";
    initCart()

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

let VerifyCheckoutIsInProgress(cart :Cart) : bool=
    if(cart.CheckoutInProgress) then true else
        printf $"Cart Checkout is not in progress, not a valid command{Environment.NewLine}"
        false
let VerifyCheckoutIsNotInProgress(cart :Cart) : bool=
    if(not cart.CheckoutInProgress) then true else
        printf $"Cart Checkout is in progress, not a valid command{Environment.NewLine}"
        false

let VerifyCheckoutIsValid (cart: Cart): bool =
    let cartState = cart.States.Peek()
    if (cartState.Items.Count = 0) then
        printf $"Can't perform checkout as cart is empty{Environment.NewLine}"
        false
    else
        true;

let VerifyEnterPersonalDetailsIValid (cart: Cart): bool =
    if cart.CheckoutInProgress = false then
        printf $"To enter your personal details, you have to first enter checkout{Environment.NewLine}"
        false
    else if not (cart.UserData = None) then
        printf $"You already entered your personal details{Environment.NewLine}"
        false
    else
        true

let VerifySelectPaymentMethodIsValid (cart: Cart): bool =
    if (not cart.CheckoutInProgress) then
        printf $"To perform a payment, you have to first enter checkout{Environment.NewLine}"
        false
    else if (cart.UserData = None) then
        printf $"To select a payment method, you have to first enter your personal details{Environment.NewLine}"
        false
    else
        true

let VerifyPaymentIsValid (cart: Cart): bool =
    if (not cart.CheckoutInProgress) then
        printf $"To perform a payment, you have to first enter checkout{Environment.NewLine}"
        false
    else if (cart.UserData = None) then
        printf $"To perform a payment, you have to first enter your personal details{Environment.NewLine}"
        false
    else if (cart.SelectedPaymentMethod = None) then
        printf $"To perform a payment, you have to select a payment method{Environment.NewLine}"
        false
    else
        true

// Message processing functions

let PrintCart (cart : Cart) : Cart =
    printf $"{cartToString cart}"
    cart

let update (msg : Message) (cart : Cart) : Cart =
    match msg with
    | Add (x,y) ->
        if VerifyCheckoutIsNotInProgress cart then addToCart cart (GetItemByIndexerFromStore x) (decimal y) else cart
    | Remove x -> if VerifyCheckoutIsNotInProgress cart then removeFromCart cart (GetItemByIndexerFromCart cart x) else cart
    | SetQuantity (x,y) -> if VerifyCheckoutIsNotInProgress cart then setQuantityInCart cart (GetItemByIndexerFromCart cart x) (decimal y) else cart
    | Undo steps -> if VerifyCheckoutIsNotInProgress cart then undoCartActions cart (decimal steps) else cart
    | PrintStoreItems -> if VerifyCheckoutIsNotInProgress cart then printItemsInStore store cart else cart
    | Checkout -> if VerifyCheckoutIsValid cart then checkout cart else cart
    | EnterPersonalDetails (name, address, email) -> if VerifyEnterPersonalDetailsIValid cart then enterPersonalDetails cart name address email else cart
    | SelectPaymentMethod index -> if VerifySelectPaymentMethodIsValid cart then selectPaymentMethod cart index else cart
    | Pay (credentialA, credentialB) -> if VerifyPaymentIsValid cart then pay cart credentialA credentialB else cart
    | PrintCart -> PrintCart cart


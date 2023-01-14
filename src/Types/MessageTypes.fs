module MessageTypes
open DomainModels

type Add = { Item: Item; Quantity: decimal }
type Remove = { Item: Item; }
type SetQuantity = { Item: Item; Quantity: decimal }

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
    | PrintCart

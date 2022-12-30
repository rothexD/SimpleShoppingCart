module MessageTypes
open DomainModels

type Add = { Item: Item; Quantity: decimal }
type Remove = { Item: Item; }
type SetQuantity = { Item: Item; Quantity: decimal }

type Message =
    | Add of Add
    | Remove of Remove
    | SetQuantity of SetQuantity
    | Undo of decimal
    | PrintStoreItems

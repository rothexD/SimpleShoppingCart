module DomainModels
open System
open System.Collections.Generic

type Item = { Id: Guid; Name: string; Price: decimal }

type ItemState = { Item: Item; Quantity: decimal }

type State = { Items: Map<Guid, ItemState>; Sum: decimal }

type Cart = { States: Stack<State> }

type Store = { Items: list<Item> }

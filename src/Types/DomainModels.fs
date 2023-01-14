module DomainModels
open System
open System.Collections.Generic

type Item = { Id: Guid; Name: string; Price: decimal }

type ItemState = { Item: Item; Quantity: decimal }

type State = { Items: Map<Guid, ItemState>; Sum: decimal }
type UserInformation = {Name:string;Address:string;Email:string}
type Credentials = {CredentialA:string;CredentialB:string}

type Cart = { States: Stack<State>;CheckoutInProgress:bool;SelectedPaymentMethod:Option<string>;UserData : Option<UserInformation>;Credentials:Option<Credentials>}

type Store = { Items: list<Item> }




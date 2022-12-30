module LoadItemsFromFile
open System.IO
open Item

let loadItemsFromFile (path : string) : list<item> =
    let ReadFromFile (path: string) : list<item> =
        System.IO.File.ReadLines(path) |> failwith ""
    if File.Exists path then ReadFromFile path else []


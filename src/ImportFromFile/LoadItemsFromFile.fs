module LoadItemsFromFile
open System
open System.IO
open Item

let loadItemsFromFile (path : string) : list<item> =
    let LineToItem(lineFromFile : string) : Option<item> =
        let subStrings = lineFromFile.Split(';')
        if(subStrings.Length <> 3) then None else
            match Guid.TryParse subStrings[0] with
                | (false, _) -> None
                | (true, result) ->
                    match Decimal.TryParse subStrings[2] with
                        | (true, result) ->  Some {Guid = Guid.Parse subStrings[0];Name = subStrings[1];Price = Decimal.Parse subStrings[2]}
                        | (false, _) -> None
    let ReadFromFile (path: string) (items : list<item>) : list<item> =
        System.IO.File.ReadAllLines(path)
        |> Array.toList
        |> List.map LineToItem
        |> List.choose id
    if File.Exists path then ReadFromFile path [] else []


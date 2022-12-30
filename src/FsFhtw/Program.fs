﻿[<EntryPoint>]
let main argv =
    printfn "Welcome to the FHTW Domain REPL!"
    printfn "Please enter your commands to interact with the system."
    printfn "Press CTRL+C to stop the program."
    printf "> "

    //example
    let items = LoadItemsFromFile.loadItemsFromFile ("ItemsInStore.txt")
    let initialState = Domain.init ()
    Repl.loop initialState
    0 // return an integer exit code

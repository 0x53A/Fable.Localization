// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =

    printfn "%s" (Loc.SR.hi("World!"))
    0 // return an integer exit code

// Learn more about F# at http://fsharp.org

open System

let SR = Loc.fromAssembly(Globalization.CultureInfo.CurrentUICulture.TwoLetterISOLanguageName)

//let iLoc : Loc.ILoc = 
//let ctx = Fable.React.Helpers.createContext(iLoc)
//let iLocReact = Loc.reactFrom(ctx)



[<EntryPoint>]
let main argv =
    printfn "%s" (SR.Current.Many(1, "2", 3, "4"))
    0 // return an integer exit code

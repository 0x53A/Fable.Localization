module WriteFs

open System.IO
open Read


let genFs (outStream:Stream) filename justfilename outFsFilename visibility printMessage (stringInfos:ParsedLine array) =
    use out = new StreamWriter(outStream)
    fprintfn out "// This is a generated file; the original input is '%s'" filename
    fprintfn out "module %s %s" visibility justfilename
    fprintfn out ""
    let interfaceName = "I" + justfilename
    fprintfn out """
open System
open System.Collections.Generic


type IResourceProvider =
    abstract member GetString : string -> string

    
type %s =""" interfaceName
    for line in stringInfos do
        let tupleType =
            match line.Holes with
            | [| |] -> "unit"
            | [| one |] -> one
            | many ->
                "(" + (many |> String.concat " * ") + ")"
        fprintfn out "    abstract member %s : %s -> string" line.Identifier tupleType

    fprintfn out """
let fromProvider (p:IResourceProvider) =
    { new %s with""" interfaceName
    for line in stringInfos do
        let args = [| for i in 0 .. line.Holes.Length - 1 -> sprintf "a%i" i |]
        fprintfn out """        member __.%s(%s) =
            let fmt = p.GetString("%s")
            String.Format(fmt%s)""" line.Identifier (args |> String.concat ", ") line.Identifier (if args = [||] then "" else ", " + (args |> String.concat ", "))
            
    fprintfn out """    }

let private failUnknownKey name =
#if DEBUG
        System.Diagnostics.Debug.Assert(false, sprintf "**RESOURCE ERROR**: Resource token %%s does not exist!" name)
#endif
        failwithf "**RESOURCE ERROR**: Resource token %%s does not exist!" name

let fromDictionary(d:Dictionary<string,string>) =
    let provider = { new IResourceProvider with
                        member x.GetString(name) =
                            match d.TryGetValue name with
                            | true, v -> v
                            | false, _ -> failUnknownKey name
                   }
    fromProvider provider

#if !NO_NETFX_RESOURCE_MANAGER

let private resources = lazy (
    let currentAssembly = System.Reflection.Assembly.GetExecutingAssembly()
    new System.Resources.ResourceManager("Loc", currentAssembly)
)
                
// newlines and tabs get converted to strings when read from a resource file
// this will preserve their original intention
let private postProcessString (s : string) =
    s.Replace("\\n","\n").Replace("\\t","\t").Replace("\\r","\r").Replace("\\\"", "\"")

let private getStringFromResources(name, cultureInfo) =
    let ci = defaultArg cultureInfo System.Globalization.CultureInfo.CurrentUICulture
    let raw =  
        let s = resources.Value.GetString(name, ci)
        if null = s then
            failUnknownKey name
        s
    postProcessString raw

let private resourceProvider = lazy (
    { new IResourceProvider with
        member x.GetString(name) = getStringFromResources(name, None)
    }
)

let fromAssembly() = fromProvider( resourceProvider.Value )
let private allKeys = [|"""

    for line in stringInfos do
        fprintfn out """    "%s";""" line.Identifier

    fprintfn out """|]

let getAllFromAssembly(cultureInfo) : IDictionary<string,string> =
    
    let d = System.Collections.Generic.Dictionary()
    for k in allKeys do
        d.Add(k, getStringFromResources(k, cultureInfo))
    d :> IDictionary<_,_>
        
#endif"""

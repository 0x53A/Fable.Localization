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


#if NO_NETFX_RESOURCE_MANAGER || FABLE_COMPILER
type Language = string
#else
type Language = System.Globalization.CultureInfo
#endif


/// support switching languages
type ILocalizationAware =
    abstract member CurrentLanguage : Language
    abstract member LanguageChanged : IEvent<unit>
    abstract member SwitchTo : Language -> unit


/// get the raw format string
type IRawStringProvider =
    abstract member GetString : Language * string -> string

/// the singleton which takes care of localization.
/// The type parameter 'T is the key interface
type ILocalizationProvider<'T> =
    inherit ILocalizationAware
    
    abstract member Current : 'T
    abstract member ForLanguage : Language -> 'T


    
type %s =""" interfaceName
    for line in stringInfos do
        let tupleType =
            match line.Holes with
            | [| |] -> "unit"
            | [| one |] -> one
            | many ->
                (many |> String.concat " * ")
        fprintfn out "    abstract member %s : %s -> string" line.Identifier tupleType

    fprintfn out """
let fromProvider (p:IRawStringProvider, l:Language) =
    { new %s with""" interfaceName
    for line in stringInfos do
        let args = [| for i in 0 .. line.Holes.Length - 1 -> sprintf "a%i" i |]
        fprintfn out """        member __.%s(%s) =
            let fmt = p.GetString(l, "%s")
            String.Format(fmt%s)""" line.Identifier (args |> String.concat ", ") line.Identifier (if args = [||] then "" else ", " + (args |> String.concat ", "))
            
    fprintfn out """    }

let private failUnknownKey name =
#if DEBUG
        System.Diagnostics.Debug.Assert(false, sprintf "**RESOURCE ERROR**: Resource token %%s does not exist!" name)
#endif
        failwithf "**RESOURCE ERROR**: Resource token %%s does not exist!" name


#if !NO_NETFX_RESOURCE_MANAGER && !FABLE_COMPILER

let private resources = lazy (
    let currentAssembly = System.Reflection.Assembly.GetExecutingAssembly()
    new System.Resources.ResourceManager("Loc", currentAssembly)
)
                
// newlines and tabs get converted to strings when read from a resource file
// this will preserve their original intention
let private postProcessString (s : string) =
    s.Replace("\\n","\n").Replace("\\t","\t").Replace("\\r","\r").Replace("\\\"", "\"")

let getStringFromResources(name, cultureInfo) =
    let ci = defaultArg cultureInfo System.Globalization.CultureInfo.CurrentUICulture
    let raw =  
        let s = resources.Value.GetString(name, ci)
        if null = s then
            failUnknownKey name
        s
    postProcessString raw

let private resourceProvider = lazy (
    { new IRawStringProvider with
        member x.GetString(lang, name) = getStringFromResources(name, Some lang)
    }
)

let fromAssembly(defaultLang:Language) : ILocalizationProvider<ILoc> =
    let mutable currentLang = defaultLang
    let mutable current = fromProvider(resourceProvider.Value, defaultLang)
    let languageChangedEvent = Event<_>()
    { new ILocalizationProvider<ILoc> with
        member __.ForLanguage(l) = fromProvider(resourceProvider.Value, l)
        member __.Current = current
      interface ILocalizationAware with
        member __.SwitchTo(l) =
            currentLang <- l
            current <- fromProvider(resourceProvider.Value, l)
        member __.LanguageChanged = languageChangedEvent.Publish
        member __.CurrentLanguage = currentLang
    }


let allKeys = [|"""

    for line in stringInfos do
        fprintfn out """    "%s";""" line.Identifier

    fprintfn out """|]

let getAllFromAssembly(cultureInfo) : IDictionary<string,string> =
    
    let d = System.Collections.Generic.Dictionary()
    for k in allKeys do
        d.Add(k, getStringFromResources(k, cultureInfo))
    d :> IDictionary<_,_>
        
#endif


#if FABLE_REACT_LOCALIZATION || FABLE_COMPILER

open Fable.React

type %sReact =""" interfaceName
    for line in stringInfos do
        let tupleType =
            match line.Holes with
            | [| |] -> "unit"
            | [| one |] -> one
            | many ->
                (many |> String.concat " * ")
        fprintfn out "    abstract member %s : %s -> ReactElement" line.Identifier tupleType

    fprintfn out """
type private LocProps<'T> = { Args : 'T }

let reactFrom (ctx:IContext<%s>) =""" interfaceName
    for line in stringInfos do
        let tupleType =
            match line.Holes with
            | [| |] -> "unit"
            | [| one |] -> one
            | many ->
                "(" + (many |> String.concat " * ") + ")"
        fprintfn out """    let f_%s =
        let render = fun (props:LocProps<%s>) ->
            let iloc = Hooks.useContext(ctx)
            str (iloc.%s(%s))
        FunctionComponent.Of(render, displayName = "LocalizedText_%s", memoizeWith = fun p1 p2 -> p1 = p2)""" line.Identifier tupleType line.Identifier (if line.Holes = [||] then "" else "props.Args") line.Identifier

    fprintfn out """

    { new %sReact with""" interfaceName

    for line in stringInfos do
        match line.Holes with
        | [| |] ->
            fprintfn out """        member this.%s() = f_%s { Args = () }""" line.Identifier line.Identifier
            
        | [| one |] ->
            fprintfn out """        member this.%s(a0) = f_%s { Args = a0 }""" line.Identifier line.Identifier
        | many ->
            let args = [| for i in 0 .. line.Holes.Length - 1 -> sprintf "a%i" i |] |> String.concat ", "
            fprintfn out """        member this.%s(%s) = f_%s { Args = (%s) }""" line.Identifier args line.Identifier args
            

    fprintfn out """
    }
#endif

"""

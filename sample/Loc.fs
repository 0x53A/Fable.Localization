// This is a generated file; the original input is 'Loc.txt'
module public Loc


    
open System
open System.Collections.Generic


type Language = string

/// If set to true, then all error messages will just return the filled 'holes' delimited by ',,,'s - this is for language-neutral testing (e.g. localization-invariant baselines).
let mutable SwallowResourceText = false

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


    
type ILoc =
    abstract member Hi : System.String -> string
    abstract member Hello : unit -> string
    abstract member Many : System.Int32 * System.String * System.Int32 * System.String -> string

let fromProvider (p:IRawStringProvider, l:Language) =
    { new ILoc with
        member __.Hi(a0) =
            let fmt =
                if SwallowResourceText then
                    ",,,{0},,,"
                else
                    p.GetString(l, "Hi")
            String.Format(fmt, a0)
        member __.Hello() =
            let fmt =
                if SwallowResourceText then
                    ",,,,,,"
                else
                    p.GetString(l, "Hello")
            String.Format(fmt)
        member __.Many(a0, a1, a2, a3) =
            let fmt =
                if SwallowResourceText then
                    ",,,{0},,,{1},,,{2},,,{3},,,"
                else
                    p.GetString(l, "Many")
            String.Format(fmt, a0, a1, a2, a3)
    }

let private failUnknownKey name =
#if DEBUG
        System.Diagnostics.Debug.Assert(false, sprintf "**RESOURCE ERROR**: Resource token %s does not exist!" name)
#endif
        failwithf "**RESOURCE ERROR**: Resource token %s does not exist!" name


#if !NO_NETFX_RESOURCE_MANAGER && !FABLE_COMPILER

let private resources = lazy (
    let currentAssembly = System.Reflection.Assembly.GetExecutingAssembly()
    new System.Resources.ResourceManager("sample.Loc", currentAssembly)
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
        member x.GetString(lang, name) = getStringFromResources(name, Some(System.Globalization.CultureInfo lang))
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


let allKeys = [|
    "Hi";
    "Hello";
    "Many";
|]

let getAllFromAssembly(cultureInfo) : IDictionary<string,string> =
    
    let d = System.Collections.Generic.Dictionary()
    for k in allKeys do
        d.Add(k, getStringFromResources(k, cultureInfo))
    d :> IDictionary<_,_>
        
#endif


#if FABLE_REACT_LOCALIZATION || FABLE_COMPILER

open Fable.React

type ILocReact =
    abstract member Hi : System.String -> ReactElement
    abstract member Hello : unit -> ReactElement
    abstract member Many : System.Int32 * System.String * System.Int32 * System.String -> ReactElement

type private LocProps<'T> = { Args : 'T }

let reactFrom (ctx:IContext<ILoc>) =
    let f_Hi =
        let render = fun (props:LocProps<System.String>) ->
            let iloc = Hooks.useContext(ctx)
            str (iloc.Hi(props.Args))
        FunctionComponent.Of(render, displayName = "LocalizedText_Hi", memoizeWith = fun p1 p2 -> p1 = p2)
    let f_Hello =
        let render = fun (props:LocProps<unit>) ->
            let iloc = Hooks.useContext(ctx)
            str (iloc.Hello())
        FunctionComponent.Of(render, displayName = "LocalizedText_Hello", memoizeWith = fun p1 p2 -> p1 = p2)
    let f_Many =
        let render = fun (props:LocProps<(System.Int32 * System.String * System.Int32 * System.String)>) ->
            let iloc = Hooks.useContext(ctx)
            str (iloc.Many(props.Args))
        FunctionComponent.Of(render, displayName = "LocalizedText_Many", memoizeWith = fun p1 p2 -> p1 = p2)


    { new ILocReact with
        member this.Hi(a0) = f_Hi { Args = a0 }
        member this.Hello() = f_Hello { Args = () }
        member this.Many(a0, a1, a2, a3) = f_Many { Args = (a0, a1, a2, a3) }

    }
#endif



module WriteFs

open System.IO
open Read


let stringBoilerPlatePrefix = @"
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
open Microsoft.FSharp.Reflection
open System.Reflection
// (namespaces below for specific case of using the tool to compile FSharp.Core itself)
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Text
open Microsoft.FSharp.Collections
open Printf
"
let StringBoilerPlate filename = @"
    // BEGIN BOILERPLATE

    static let getCurrentAssembly () = System.Reflection.Assembly.GetExecutingAssembly()

    static let getTypeInfo (t: System.Type) = t

    static let resources = lazy (new System.Resources.ResourceManager(""" + filename + @""", getCurrentAssembly()))

    static let GetString(name:string) =
        let s = resources.Value.GetString(name, System.Globalization.CultureInfo.CurrentUICulture)
    #if DEBUG
        if null = s then
            System.Diagnostics.Debug.Assert(false, sprintf ""**RESOURCE ERROR**: Resource token %s does not exist!"" name)
    #endif
        s

    static let mkFunctionValue (tys: System.Type[]) (impl:obj->obj) = 
        FSharpValue.MakeFunction(FSharpType.MakeFunctionType(tys.[0],tys.[1]), impl)

    static let funTyC = typeof<(obj -> obj)>.GetGenericTypeDefinition()  

    static let isNamedType(ty:System.Type) = not (ty.IsArray ||  ty.IsByRef ||  ty.IsPointer)
    static let isFunctionType (ty1:System.Type)  =
        isNamedType(ty1) && getTypeInfo(ty1).IsGenericType && (ty1.GetGenericTypeDefinition()).Equals(funTyC)

    static let rec destFunTy (ty:System.Type) =
        if isFunctionType ty then
            ty, ty.GetGenericArguments() 
        else
            match getTypeInfo(ty).BaseType with 
            | null -> failwith ""destFunTy: not a function type""
            | b -> destFunTy b 

    static let buildFunctionForOneArgPat (ty: System.Type) impl =
        let _,tys = destFunTy ty
        let rty = tys.[1]
        // PERF: this technique is a bit slow (e.g. in simple cases, like 'sprintf ""%x""')
        mkFunctionValue tys (fun inp -> impl rty inp)

    static let capture1 (fmt:string) i args ty (go : obj list -> System.Type -> int -> obj) : obj =
        match fmt.[i] with
        | '%' -> go args ty (i+1)
        | 'd'
        | 'f'
        | 's' -> buildFunctionForOneArgPat ty (fun rty n -> go (n :: args) rty (i+1))
        | _ -> failwith ""bad format specifier""

    // newlines and tabs get converted to strings when read from a resource file
    // this will preserve their original intention
    static let postProcessString (s : string) =
        s.Replace(""\\n"",""\n"").Replace(""\\t"",""\t"").Replace(""\\r"",""\r"").Replace(""\\\"""", ""\"""")

    static let createMessageString (messageString : string) (fmt : Printf.StringFormat<'T>) : 'T =
        let fmt = fmt.Value // here, we use the actual error string, as opposed to the one stored as fmt
        let len = fmt.Length 

        /// Function to capture the arguments and then run.
        let rec capture args ty i =
            if i >= len ||  (fmt.[i] = '%' && i+1 >= len) then
                let b = new System.Text.StringBuilder()
                b.AppendFormat(messageString, [| for x in List.rev args -> x |]) |> ignore
                box(b.ToString())
            // REVIEW: For these purposes, this should be a nop, but I'm leaving it
            // in incase we ever decide to support labels for the error format string
            // E.g., ""<name>%s<foo>%d""
            elif System.Char.IsSurrogatePair(fmt,i) then
                capture args ty (i+2)
            else
                match fmt.[i] with
                | '%' ->
                    let i = i+1
                    capture1 fmt i args ty capture
                | _ ->
                    capture args ty (i+1)

        (unbox (capture [] (typeof<'T>) 0) : 'T)

    static let mutable swallowResourceText = false

    static let GetStringFunc((messageID : string),(fmt : Printf.StringFormat<'T>)) : 'T =
        if swallowResourceText then
            sprintf fmt
        else
            let mutable messageString = GetString(messageID)
            messageString <- postProcessString messageString
            createMessageString messageString fmt

    /// If set to true, then all error messages will just return the filled 'holes' delimited by ',,,'s - this is for language-neutral testing (e.g. localization-invariant baselines).
    static member SwallowResourceText with get () = swallowResourceText
                                        and set (b) = swallowResourceText <- b
    // END BOILERPLATE
"

let genFs (outStream:Stream) filename justfilename outFsFilename visibility printMessage (stringInfos:ParsedLine array) =
    use out = new StreamWriter(outStream)
    fprintfn out "// This is a generated file; the original input is '%s'" filename
    fprintfn out "namespace %s" justfilename
    fprintfn out "%s" stringBoilerPlatePrefix
    fprintfn out "type %s SR private() =" visibility
    let theResourceName = justfilename
    fprintfn out "%s" (StringBoilerPlate theResourceName)

    printMessage (sprintf "Generating resource methods for %s" outFsFilename)
    // gen each resource method
    stringInfos |> Seq.iter (fun line ->
        let formalArgs = new System.Text.StringBuilder()
        let actualArgs = new System.Text.StringBuilder()
        let firstTime = ref true
        let n = ref 0
        formalArgs.Append "(" |> ignore
        for hole in line.Holes do
            if !firstTime then
                firstTime := false
            else
                formalArgs.Append ", " |> ignore
                actualArgs.Append " " |> ignore
            formalArgs.Append(sprintf "a%d : %s" !n hole) |> ignore
            actualArgs.Append(sprintf "a%d" !n) |> ignore
            n := !n + 1
        formalArgs.Append ")" |> ignore
        fprintfn out "    /// %s" line.OriginalString
        fprintfn out "    /// (Originally from %s:%d)" filename (line.LineNumber+1)
        let justPercentsFromFormatString = 
            (line.Holes |> Array.fold (fun acc holeType -> 
                acc + match holeType with 
                        | "System.Int32" -> ",,,%d" 
                        | "System.Double" -> ",,,%f" 
                        | "System.String" -> ",,,%s"
                        | _ -> failwith "unreachable") "") + ",,,"
        let errPrefix = match line.ErrorNumber with
                        | None -> ""
                        | Some n -> sprintf "%d, " n
        fprintfn out "    static member %s%s = (%sGetStringFunc(\"%s\",\"%s\") %s)" line.Identifier (formalArgs.ToString()) errPrefix line.Identifier justPercentsFromFormatString (actualArgs.ToString())
        )

    fprintfn out ""
    // gen validation method
    fprintfn out "    /// Call this method once to validate that all known resources are valid; throws if not"
    fprintfn out "    static member RunStartupValidation() ="
    stringInfos |> Seq.iter (fun line ->
        fprintfn out "        ignore(GetString(\"%s\"))" line.Identifier
    )
    fprintfn out "        ()"  // in case there are 0 strings, we need the generated code to parse
    out.Flush()
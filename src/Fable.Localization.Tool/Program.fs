open System
open System.IO

type InLine = {
    TxtFilePath : string
    LogicalName : string
    SourceVisibility : string
    SourceOutputFilePath : string
    ResourceOutputFilePath : string
}

type OutLine = {
    TxtPath : string
    LogicalName : string
    FsPath : string
    ResxPath : string
    AutoIncludeFs : string
    AutoIncludeResx : string
}


// copied from https://github.com/dotnet/fsharp/blob/ccb913d3a05863e5b1861d64994ffb97ed498855/src/fsharp/FSharp.Build/FSharpEmbedResourceText.fs

let transform (input : InLine) objDir : OutLine =

    let PrintErr(filename, line, msg) =
        printfn "%s(%d): error : %s" filename line msg

    let Err(filename, line, msg) =
        PrintErr(filename, line, msg)
        printfn "Note that the syntax of each line is one of these three alternatives:"
        printfn "# comment"
        printfn "ident,\"string\""
        printfn "errNum,ident,\"string\""
        failwith (sprintf "there were errors in the file '%s'" filename)

    let xmlBoilerPlateString = @"<?xml version=""1.0"" encoding=""utf-8""?>
<root>
    <!--
    Microsoft ResX Schema 

    Version 2.0

    The primary goals of this format is to allow a simple XML format
    that is mostly human readable. The generation and parsing of the
    various data types are done through the TypeConverter classes
    associated with the data types.

    Example:

    ... ado.net/XML headers & schema ...
    <resheader name=""resmimetype"">text/microsoft-resx</resheader>
    <resheader name=""version"">2.0</resheader>
    <resheader name=""reader"">System.Resources.ResXResourceReader, System.Windows.Forms, ...</resheader>
    <resheader name=""writer"">System.Resources.ResXResourceWriter, System.Windows.Forms, ...</resheader>
    <data name=""Name1""><value>this is my long string</value><comment>this is a comment</comment></data>
    <data name=""Color1"" type=""System.Drawing.Color, System.Drawing"">Blue</data>
    <data name=""Bitmap1"" mimetype=""application/x-microsoft.net.object.binary.base64"">
        <value>[base64 mime encoded serialized .NET Framework object]</value>
    </data>
    <data name=""Icon1"" type=""System.Drawing.Icon, System.Drawing"" mimetype=""application/x-microsoft.net.object.bytearray.base64"">
        <value>[base64 mime encoded string representing a byte array form of the .NET Framework object]</value>
        <comment>This is a comment</comment>
    </data>

    There are any number of ""resheader"" rows that contain simple 
    name/value pairs.

    Each data row contains a name, and value. The row also contains a
    type or mimetype. Type corresponds to a .NET class that support
    text/value conversion through the TypeConverter architecture.
    Classes that don't support this are serialized and stored with the
    mimetype set.

    The mimetype is used for serialized objects, and tells the
    ResXResourceReader how to depersist the object. This is currently not
    extensible. For a given mimetype the value must be set accordingly:

    Note - application/x-microsoft.net.object.binary.base64 is the format
    that the ResXResourceWriter will generate, however the reader can
    read any of the formats listed below.

    mimetype: application/x-microsoft.net.object.binary.base64
    value   : The object must be serialized with
            : System.Runtime.Serialization.Formatters.Binary.BinaryFormatter
            : and then encoded with base64 encoding.

    mimetype: application/x-microsoft.net.object.soap.base64
    value   : The object must be serialized with
            : System.Runtime.Serialization.Formatters.Soap.SoapFormatter
            : and then encoded with base64 encoding.

    mimetype: application/x-microsoft.net.object.bytearray.base64
    value   : The object must be serialized into a byte array
            : using a System.ComponentModel.TypeConverter
            : and then encoded with base64 encoding.
    -->
    <xsd:schema id=""root"" xmlns="""" xmlns:xsd=""http://www.w3.org/2001/XMLSchema"" xmlns:msdata=""urn:schemas-microsoft-com:xml-msdata"">
    <xsd:import namespace=""http://www.w3.org/XML/1998/namespace"" />
    <xsd:element name=""root"" msdata:IsDataSet=""true"">
        <xsd:complexType>
        <xsd:choice maxOccurs=""unbounded"">
            <xsd:element name=""metadata"">
            <xsd:complexType>
                <xsd:sequence>
                <xsd:element name=""value"" type=""xsd:string"" minOccurs=""0"" />
                </xsd:sequence>
                <xsd:attribute name=""name"" use=""required"" type=""xsd:string"" />
                <xsd:attribute name=""type"" type=""xsd:string"" />
                <xsd:attribute name=""mimetype"" type=""xsd:string"" />
                <xsd:attribute ref=""xml:space"" />
            </xsd:complexType>
            </xsd:element>
            <xsd:element name=""assembly"">
            <xsd:complexType>
                <xsd:attribute name=""alias"" type=""xsd:string"" />
                <xsd:attribute name=""name"" type=""xsd:string"" />
            </xsd:complexType>
            </xsd:element>
            <xsd:element name=""data"">
            <xsd:complexType>
                <xsd:sequence>
                <xsd:element name=""value"" type=""xsd:string"" minOccurs=""0"" msdata:Ordinal=""1"" />
                <xsd:element name=""comment"" type=""xsd:string"" minOccurs=""0"" msdata:Ordinal=""2"" />
                </xsd:sequence>
                <xsd:attribute name=""name"" type=""xsd:string"" use=""required"" msdata:Ordinal=""1"" />
                <xsd:attribute name=""type"" type=""xsd:string"" msdata:Ordinal=""3"" />
                <xsd:attribute name=""mimetype"" type=""xsd:string"" msdata:Ordinal=""4"" />
                <xsd:attribute ref=""xml:space"" />
            </xsd:complexType>
            </xsd:element>
            <xsd:element name=""resheader"">
            <xsd:complexType>
                <xsd:sequence>
                <xsd:element name=""value"" type=""xsd:string"" minOccurs=""0"" msdata:Ordinal=""1"" />
                </xsd:sequence>
                <xsd:attribute name=""name"" type=""xsd:string"" use=""required"" />
            </xsd:complexType>
            </xsd:element>
        </xsd:choice>
        </xsd:complexType>
    </xsd:element>
    </xsd:schema>
    <resheader name=""resmimetype"">
        <value>text/microsoft-resx</value>
    </resheader>
    <resheader name=""version"">
        <value>2.0</value>
    </resheader>
    <resheader name=""reader"">
        <value>System.Resources.ResXResourceReader, System.Windows.Forms, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089</value>
    </resheader>
    <resheader name=""writer"">
        <value>System.Resources.ResXResourceWriter, System.Windows.Forms, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089</value>
    </resheader>
</root>"

    // The kinds of 'holes' we can do
    let ComputeHoles filename lineNum (txt:string) : ResizeArray<string> * string =
        // takes in a %d%s kind of string, returns array of string and {0}{1} kind of string
        let mutable i = 0
        let mutable holeNumber = 0
        let mutable holes = ResizeArray()  //  order
        let sb = new System.Text.StringBuilder()
        let AddHole holeType =
            sb.Append(sprintf "{%d}" holeNumber) |> ignore
            holeNumber <- holeNumber + 1
            holes.Add(holeType)
        while i < txt.Length do
            if txt.[i] = '%' then
                if i+1 = txt.Length then
                    Err(filename, lineNum, "(at end of string) % must be followed by d, f, s, or %")
                else
                    match txt.[i+1] with
                    | 'd' -> AddHole "System.Int32"
                    | 'f' -> AddHole "System.Double"
                    | 's' -> AddHole "System.String"
                    | '%' -> sb.Append('%') |> ignore
                    | c -> Err(filename, lineNum, sprintf "'%%%c' is not a valid sequence, only %%d %%f %%s or %%%%" c)
                i <- i + 2
            else
                match txt.[i] with
                | '{' -> sb.Append "{{" |> ignore
                | '}' -> sb.Append "}}" |> ignore
                | c -> sb.Append c |> ignore
                i <- i + 1
        //printfn "holes.Length = %d, lineNum = %d" holes.Length //lineNum txt
        (holes, sb.ToString())

    let Unquote (s : string) =
        if s.StartsWith "\"" && s.EndsWith "\"" then s.Substring(1, s.Length - 2)
        else failwith "error message string should be quoted"

    let ParseLine filename lineNum (txt:string) =
        let mutable errNum = None
        let identB = new System.Text.StringBuilder()
        let mutable i = 0
        // parse optional error number
        if i < txt.Length && System.Char.IsDigit txt.[i] then
            let numB = new System.Text.StringBuilder()
            while i < txt.Length && System.Char.IsDigit txt.[i] do
                numB.Append txt.[i] |> ignore
                i <- i + 1
            errNum <- Some(int (numB.ToString()))
            if i = txt.Length || txt.[i] <> ',' then
                Err(filename, lineNum, sprintf "After the error number '%d' there should be a comma" errNum.Value)
            // Skip the comma
            i <- i + 1
        // parse short identifier
        if i < txt.Length && not(System.Char.IsLetter(txt.[i])) then
            Err(filename, lineNum, sprintf "The first character in the short identifier should be a letter, but found '%c'" txt.[i])
        while i < txt.Length && System.Char.IsLetterOrDigit txt.[i] do
            identB.Append txt.[i] |> ignore
            i <- i + 1
        let ident = identB.ToString()
        if ident.Length = 0 then
            Err(filename, lineNum, "Did not find the short identifier")
        else
            if i = txt.Length || txt.[i] <> ',' then
                Err(filename, lineNum, sprintf "After the identifier '%s' there should be a comma" ident)
            else
                // Skip the comma
                i <- i + 1
                if i = txt.Length then
                    Err(filename, lineNum, sprintf "After the identifier '%s' and comma, there should be the quoted string resource" ident)
                else
                    let str = 
                        try
                            System.String.Format(Unquote(txt.Substring i))  // Format turns e.g '\n' into that char, but also requires that we 'escape' curlies in the original .txt file, e.g. "{{"
                        with 
                            e -> Err(filename, lineNum, sprintf "Error calling System.String.Format (note that curly braces must be escaped, and there cannot be trailing space on the line): >>>%s<<< -- %s" (txt.Substring i) e.Message)
                    let holes, netFormatString = ComputeHoles filename lineNum str
                    (lineNum, (errNum,ident), str, holes.ToArray(), netFormatString)

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

    // -----------------------------------------------------------------------------------------------------------------
    // START Processing
    // -----------------------------------------------------------------------------------------------------------------

    let filename = input.TxtFilePath
    let justfilename = Path.GetFileNameWithoutExtension(filename) // .txt
    let outFilename, autoIncludeFs =
        if String.IsNullOrWhiteSpace input.SourceOutputFilePath then
            Path.Combine(objDir, justfilename + ".fs"), true
        else
            input.SourceOutputFilePath, false
    let outXmlFilename, autoIncludeResx =
        if String.IsNullOrWhiteSpace input.ResourceOutputFilePath then
            Path.Combine(objDir, justfilename + ".resx"), true
        else
            input.ResourceOutputFilePath, false

    let visibility =
        match input.SourceVisibility with
        | "Public" -> "public"
        | _ -> "internal"

    let printMessage message = printfn "FSharpEmbedResourceText: %s" message
    if justfilename |> Seq.exists (System.Char.IsLetterOrDigit >> not) then
        Err(filename, 0, sprintf "The filename '%s' is not allowed; only letters and digits can be used, as the filename also becomes the namespace for the SR class" justfilename)
        
    printMessage (sprintf "Reading %s" filename)
    let lines = File.ReadAllLines(filename) 
                |> Array.mapi (fun i s -> i,s) // keep line numbers
                |> Array.filter (fun (i,s) -> not(s.StartsWith "#"))  // filter out comments

    printMessage (sprintf "Parsing %s" filename)
    let stringInfos = lines |> Array.map (fun (i,s) -> ParseLine filename i s)
    // now we have array of (lineNum, ident, str, holes, netFormatString)  // str has %d, netFormatString has {0}
            
    printMessage (sprintf "Validating %s" filename)
    // validate that all the idents are unique
    let allIdents = new System.Collections.Generic.Dictionary<string,int>()
    for (line,(_,ident),_,_,_) in stringInfos do
        if allIdents.ContainsKey(ident) then
            Err(filename,line,sprintf "Identifier '%s' is already used previously on line %d - each identifier must be unique" ident allIdents.[ident])
        allIdents.Add(ident,line)
            
    printMessage (sprintf "Validating uniqueness of %s" filename)
    // validate that all the strings themselves are unique
    let allStrs = new System.Collections.Generic.Dictionary<string,(int*string)>()
    for (line,(_,ident),str,_,_) in stringInfos do
        if allStrs.ContainsKey(str) then
            let prevLine,prevIdent = allStrs.[str]
            Err(filename,line,sprintf "String '%s' already appears on line %d with identifier '%s' - each string must be unique" str prevLine prevIdent)
        allStrs.Add(str,(line,ident))
            
    printMessage (sprintf "Generating %s" outFilename)
    use outStream = new MemoryStream()
    use out = new StreamWriter(outStream)
    fprintfn out "// This is a generated file; the original input is '%s'" filename
    fprintfn out "namespace %s" justfilename
    fprintfn out "%s" stringBoilerPlatePrefix
    fprintfn out "type %s SR private() =" visibility
    let theResourceName = justfilename
    fprintfn out "%s" (StringBoilerPlate theResourceName)

    printMessage (sprintf "Generating resource methods for %s" outFilename)
    // gen each resource method
    stringInfos |> Seq.iter (fun (lineNum, (optErrNum,ident), str, holes, netFormatString) ->
        let formalArgs = new System.Text.StringBuilder()
        let actualArgs = new System.Text.StringBuilder()
        let firstTime = ref true
        let n = ref 0
        formalArgs.Append "(" |> ignore
        for hole in holes do
            if !firstTime then
                firstTime := false
            else
                formalArgs.Append ", " |> ignore
                actualArgs.Append " " |> ignore
            formalArgs.Append(sprintf "a%d : %s" !n hole) |> ignore
            actualArgs.Append(sprintf "a%d" !n) |> ignore
            n := !n + 1
        formalArgs.Append ")" |> ignore
        fprintfn out "    /// %s" str
        fprintfn out "    /// (Originally from %s:%d)" filename (lineNum+1)
        let justPercentsFromFormatString = 
            (holes |> Array.fold (fun acc holeType -> 
                acc + match holeType with 
                        | "System.Int32" -> ",,,%d" 
                        | "System.Double" -> ",,,%f" 
                        | "System.String" -> ",,,%s"
                        | _ -> failwith "unreachable") "") + ",,,"
        let errPrefix = match optErrNum with
                        | None -> ""
                        | Some n -> sprintf "%d, " n
        fprintfn out "    static member %s%s = (%sGetStringFunc(\"%s\",\"%s\") %s)" ident (formalArgs.ToString()) errPrefix ident justPercentsFromFormatString (actualArgs.ToString())
        )

    printMessage (sprintf "Generating .resx for %s" outFilename)
    fprintfn out ""
    // gen validation method
    fprintfn out "    /// Call this method once to validate that all known resources are valid; throws if not"
    fprintfn out "    static member RunStartupValidation() ="
    stringInfos |> Seq.iter (fun (lineNum, (optErrNum,ident), str, holes, netFormatString) ->
        fprintfn out "        ignore(GetString(\"%s\"))" ident
    )
    fprintfn out "        ()"  // in case there are 0 strings, we need the generated code to parse
    out.Flush()
    // gen to resx
    let xd = new System.Xml.XmlDocument()
    xd.LoadXml(xmlBoilerPlateString)
    stringInfos |> Seq.iter (fun (lineNum, (optErrNum,ident), str, holes, netFormatString) ->
        let xn = xd.CreateElement("data")
        xn.SetAttribute("name",ident) |> ignore
        xn.SetAttribute("xml:space","preserve") |> ignore
        let xnc = xd.CreateElement "value"
        xn.AppendChild xnc |> ignore
        xnc.AppendChild(xd.CreateTextNode netFormatString) |> ignore
        xd.LastChild.AppendChild xn |> ignore
    )
    use outXmlStream = new MemoryStream()
    xd.Save outXmlStream
    printMessage (sprintf "Done %s" outFilename)

    // save or touch
    let fsBytes = outStream.ToArray()
    let resxBytes = outXmlStream.ToArray()
    let inputFileTimestamp = File.GetLastWriteTime(input.TxtFilePath)

    let overwriteOrTouch file content =
        if File.Exists file then
            let oldContent = File.ReadAllBytes file
            if oldContent = content then
                // touch
                File.SetLastWriteTime(file, inputFileTimestamp.AddSeconds(1.))
            else
                File.WriteAllBytes(file, content)
        else
            File.WriteAllBytes(file, content)
            
    overwriteOrTouch outFilename fsBytes
    overwriteOrTouch outXmlFilename resxBytes

    {
        TxtPath = input.TxtFilePath
        LogicalName = input.LogicalName
        FsPath = outFilename
        ResxPath = outXmlFilename
        AutoIncludeFs = if autoIncludeFs then "true" else "false"
        AutoIncludeResx = if autoIncludeResx then "true" else "false"
    }

[<EntryPoint>]
let main argv =
    if argv.Length <> 3 then
        eprintfn "Error: Expected two arguments (in-file, out-file, obj-dir), but got %i: %A" argv.Length argv
        exit 1

    try

        let inFilePath = argv.[0]
        let outFilePath = argv.[1]
        let objDir = argv.[2]

        let inFileLines =
            File.ReadAllLines inFilePath
            |> Array.filter (not << String.IsNullOrWhiteSpace)

        let inFiles = [|
            for l in inFileLines ->
            let segments = l.Split(';')
            if segments.Length <> 5 then
                failwithf "Error: Malformed input file. Expected 5 semi-colon sperated columns, but got %i: %A" segments.Length segments
            
            let r = {
                TxtFilePath = segments.[0]
                LogicalName = segments.[1]
                SourceVisibility = segments.[2]
                SourceOutputFilePath = segments.[3]
                ResourceOutputFilePath = segments.[4]
            }

            // validate
            if not (File.Exists r.TxtFilePath) then
                failwithf "Error: File '%s' does not exist." r.TxtFilePath
            if r.SourceVisibility <> "" && r.SourceVisibility <> "Public" && r.SourceVisibility <> "Internal" then
                failwithf "Error: Entry '%s' has an invalid 'SourceVisibility'. Expected 'Public' or 'Internal' but got '%s'" r.TxtFilePath r.SourceVisibility
                
            r
        |]

        let outRows = [|
            for r in inFiles -> transform r objDir
        |]

        let outLines = [|
            for r in outRows -> sprintf "%s;%s;%s;%s;%s;%s" r.TxtPath r.LogicalName r.FsPath r.ResxPath r.AutoIncludeFs r.AutoIncludeResx
        |]

        File.WriteAllLines(outFilePath, outLines)

        0

    with
      exn ->
        eprintfn "%s" exn.Message
        1
open System
open System.IO

open Argu

open Read
open WriteResx
open WriteFs

type CLI_Args =
| [<ArguAttributes.ExactlyOnceAttribute>] Input of string
| [<ArguAttributes.ExactlyOnceAttribute>] Output of string
| [<ArguAttributes.ExactlyOnceAttribute>] ObjDir of string
| [<ArguAttributes.ExactlyOnceAttribute>] RootNamespace of string
| [<ArguAttributes.ExactlyOnceAttribute>] InputFiles of string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "The command file which contains one line per file to parse."
            | Output _ -> "The output file, for each input the tool will output one line."
            | ObjDir _ -> "The 'IntermediateOutputPath' of the project."
            | RootNamespace _ -> "The 'RootNamespace' of the project."
            | InputFiles _ -> "All input files, the tool will calculate the timestamp from them."

/// one input row when called from msbuild
type InputRow = {
    TxtFilePath : string
    LogicalName : string
    SourceVisibility : string
    SourceOutputFilePath : string
    ResourceOutputFilePath : string
}

/// one output row returning to msbuild
type OutputRow = {
    TxtPath : string
    LogicalName : string
    FsPath : string
    ResxPath : string
    AutoIncludeFs : string
    AutoIncludeResx : string
}


// copied from https://github.com/dotnet/fsharp/blob/ccb913d3a05863e5b1861d64994ffb97ed498855/src/fsharp/FSharp.Build/FSharpEmbedResourceText.fs

let transform (input : InputRow) objDir rootNamespace timestamp : OutputRow =

    // -----------------------------------------------------------------------------------------------------------------
    // START Processing
    // -----------------------------------------------------------------------------------------------------------------

    let filename = input.TxtFilePath
    let justfilename = Path.GetFileNameWithoutExtension(filename) // .txt
    let outFsFilename, autoIncludeFs =
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
    for line in stringInfos do
        if allIdents.ContainsKey(line.Identifier) then
            Err(filename,line.LineNumber,sprintf "Identifier '%s' is already used previously on line %d - each identifier must be unique" line.Identifier allIdents.[line.Identifier])
        allIdents.Add(line.Identifier,line.LineNumber)
            
    printMessage (sprintf "Validating uniqueness of %s" filename)
    // validate that all the strings themselves are unique
    let allStrs = new System.Collections.Generic.Dictionary<string,(int*string)>()
    for line in stringInfos do
        if allStrs.ContainsKey(line.OriginalString) then
            let prevLine,prevIdent = allStrs.[line.OriginalString]
            Err(filename,line.LineNumber,sprintf "String '%s' already appears on line %d with identifier '%s' - each string must be unique" line.OriginalString prevLine prevIdent)
        allStrs.Add(line.OriginalString,(line.LineNumber,line.Identifier))
            
    printMessage (sprintf "Generating %s" outFsFilename)
    use outStream = new MemoryStream()
    genFs outStream filename justfilename outFsFilename visibility printMessage stringInfos rootNamespace
    
    printMessage (sprintf "Generating .resx for %s" outFsFilename)
    use outXmlStream = new MemoryStream()
    genResx outXmlStream stringInfos

    // save or touch
    let fsBytes = outStream.ToArray()
    let resxBytes = outXmlStream.ToArray()
    //let inputFileTimestamp = File.GetLastWriteTime(input.TxtFilePath)

    let overwriteOrTouch file content =
        if File.Exists file then
            let oldContent = File.ReadAllBytes file
            if oldContent = content then
                // touch
                let ts = File.GetLastWriteTime file
                if ts < timestamp then
                    File.SetLastWriteTime(file, DateTime.Now)
            else
                File.WriteAllBytes(file, content)
        else
            File.WriteAllBytes(file, content)
            
    overwriteOrTouch outFsFilename fsBytes
    overwriteOrTouch outXmlFilename resxBytes

    printMessage (sprintf "Done %s" outFsFilename)

    {
        TxtPath = input.TxtFilePath
        LogicalName = input.LogicalName
        FsPath = outFsFilename
        ResxPath = outXmlFilename
        AutoIncludeFs = if autoIncludeFs then "true" else "false"
        AutoIncludeResx = if autoIncludeResx then "true" else "false"
    }

[<EntryPoint>]
let main argv =

    try

        let parser = Argu.ArgumentParser.Create<CLI_Args>()
        let cli = parser.ParseCommandLine()

        let inFilePath = cli.GetResult CLI_Args.Input
        let outFilePath = cli.GetResult CLI_Args.Output
        let objDir = cli.GetResult CLI_Args.ObjDir
        let rootNamespace = cli.GetResult CLI_Args.RootNamespace
        let inputsString = cli.GetResult CLI_Args.InputFiles
        let inputs = inputsString.Split(';')

        let timestamp = [ for i in inputs -> File.GetLastWriteTime i ] |> Seq.max

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
            for r in inFiles -> transform r objDir rootNamespace timestamp
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
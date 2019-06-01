module Read


/// one line from a txt file
type ParsedLine = {
    LineNumber : int
    ErrorNumber : int option
    Identifier : string
    OriginalString : string
    Holes : string array
    NetFormatString : string
}



let PrintErr(filename, line, msg) =
    printfn "%s(%d): error : %s" filename line msg

let Err(filename, line, msg) =
    PrintErr(filename, line, msg)
    printfn "Note that the syntax of each line is one of these three alternatives:"
    printfn "# comment"
    printfn "ident,\"string\""
    printfn "errNum,ident,\"string\""
    failwith (sprintf "there were errors in the file '%s'" filename)

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

let ParseLine filename lineNum (txt:string) : ParsedLine =
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
                { LineNumber = lineNum; ErrorNumber = errNum; Identifier = ident; OriginalString = str; Holes = holes.ToArray(); NetFormatString = netFormatString }

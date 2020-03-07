module WriteJs

open Read
open System.IO
open System.Collections.Generic
open System.Text
open System.Windows.Forms
open System.Resources
open System.Collections

let genJs (outJsStream:Stream) (stringInfos:ParsedLine array) (pathToNeutralResx:string) =

    let fi = FileInfo(pathToNeutralResx)
    let pathToNeutralResx = fi.FullName

    let ext = Path.GetExtension(pathToNeutralResx)
    if ext <> ".resx" then failwithf "expected 'pathToNeutralResx' to end with '.resx', but it ends with '%s'" ext

    let filenamePart = Path.GetFileNameWithoutExtension(pathToNeutralResx)
    let dirPart = Path.GetDirectoryName(pathToNeutralResx)

    let allSpecializedResx = [|
        for fullPath in Directory.GetFiles(dirPart, filenamePart + ".*.resx") ->
            let filename = Path.GetFileNameWithoutExtension(fullPath)
            let lang = filename.Substring(filenamePart.Length + 1)
            lang, fullPath
    |]

    let loadDictFromResx(fullPath:string) =
        use reader = new ResXResourceReader(fullPath)
        let entries = [
            for entry : DictionaryEntry in reader |> Seq.cast<DictionaryEntry> do
                match entry.Key, entry.Value with
                | :? string as k, :? string as v ->
                    yield k, v
                | _, _ -> ()
        ]
        Map entries

    let neutralKVs =  loadDictFromResx pathToNeutralResx
    let resourcesDict = Dictionary()
    for lang, fullPath in allSpecializedResx do
        let languageSpecificKVs = loadDictFromResx fullPath
        // merge the two dicts
        resourcesDict.[lang] <- Map(Seq.concat [ (Map.toSeq neutralKVs) ; (Map.toSeq languageSpecificKVs) ]) 
    resourcesDict.[""] <- neutralKVs

    let jsonForDict = Newtonsoft.Json.JsonConvert.SerializeObject(resourcesDict)

    let fullJson = "export default " + jsonForDict + ";"

    use sw = new StreamWriter(outJsStream, Encoding.UTF8)
    sw.Write(fullJson)

#r "paket: 
storage: none
source https://api.nuget.org/v3/index.json

nuget FSharp.Core 4.3.4 // https://github.com/fsharp/FAKE/issues/2001
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.BuildServer.AppVeyor
//"
#load "./.fake/build.fsx/intellisense.fsx"

#if !FAKE
#r "netstandard"
#endif

open System
open System.Diagnostics

open Fake.Core
open Fake.DotNet
open Fake.IO
open System.Threading
open System.IO
open Fake.Core

// Init buildserver
Fake.Core.BuildServer.install [ Fake.BuildServer.AppVeyor.Installer ]

let (@@) a b = Path.combine a b

let root = __SOURCE_DIRECTORY__

let inline withWorkDir wd = DotNet.Options.withWorkingDirectory wd


let packOne outDir fsproj v =
    DotNet.pack (fun o ->
        { o with    Configuration = DotNet.BuildConfiguration.Release
                    OutputPath = Some outDir
                    MSBuildParams = { MSBuild.CliArguments.Create() with Properties = ["Version",v; "PackageVersion",v] }
        }) fsproj

let pushOne f =
    DotNet.nugetPush (fun o -> { o with
                                    PushParams = { o.PushParams with
                                                                    Source = Some "https://api.nuget.org/v3/index.json"
                                                                    ApiKey = Some (Fake.Core.Environment.environVarOrFail "NUGET_API_KEY") } }) f

let justBuild() =
    let v = "0.0.1-local"

    // dotnet pack does a build
    packOne (root @@ "nupkg") (root @@ "src\\Fable.Localization.Tool\\Fable.Localization.Tool.fsproj") v
    packOne (root @@ "nupkg") (root @@ "src\\Fable.Localization.Lib\\Fable.Localization.Lib.fsproj") v

    do
        let props = ["NuspecFile","nuspec\\Fable.Localization.nuspec"; "nuspecproperties",sprintf"version=%s" v;"PackageOutputPath",(root @@ "nupkg")]
        DotNet.msbuild (fun o -> { o with MSBuildParams = { o.MSBuildParams with Targets=["Restore";"Build"];Properties = props }}) (root @@ "src\\Fable.Localization\\Fable.Localization.csproj")
        DotNet.msbuild (fun o -> { o with MSBuildParams = { o.MSBuildParams with Targets=["pack"];Properties = props }}) (root @@ "src\\Fable.Localization\\Fable.Localization.csproj")

let publishToNuget(v:string) =

    // dotnet pack does a build
    packOne (root @@ "nupkg") (root @@ "src\\Fable.Localization.Tool\\Fable.Localization.Tool.fsproj") v
    packOne (root @@ "nupkg") (root @@ "src\\Fable.Localization.Lib\\Fable.Localization.Lib.fsproj") v
    
    do
        let props = ["NuspecFile","nuspec\\Fable.Localization.nuspec"; "nuspecproperties",sprintf"version=%s" v;"PackageOutputPath",(root @@ "nupkg")]
        DotNet.msbuild (fun o -> { o with MSBuildParams = { o.MSBuildParams with Targets=["Restore";"Build"];Properties = props }}) (root @@ "src\\Fable.Localization\\Fable.Localization.csproj")
        DotNet.msbuild (fun o -> { o with MSBuildParams = { o.MSBuildParams with Targets=["pack"];Properties = props }}) (root @@ "src\\Fable.Localization\\Fable.Localization.csproj")

    let nupkgs = [|
        root @@ "nupkg" @@ ("Fable.Localization.Tool." + v + ".nupkg")
        root @@ "nupkg" @@ ("Fable.Localization.Lib." + v + ".nupkg")
        root @@ "nupkg" @@ ("Fable.Localization." + v + ".nupkg")
    |]

    for f in nupkgs do pushOne f


Target.create "BuildRelease" (fun _ ->
    justBuild()
)

Target.create "AppVeyor" (fun _ ->
    if not (Fake.BuildServer.AppVeyor.detect()) then
        failwith "expected to be run on appveyor"

    if Fake.BuildServer.AppVeyor.Environment.RepoTag then
        let tagName = Fake.BuildServer.AppVeyor.Environment.RepoTagName
        if tagName.StartsWith("release-") then
            let version = tagName.Substring("release-".Length)
            publishToNuget(version)
        else if tagName.StartsWith("prerelease-") then
            let version = tagName.Substring("prerelease-".Length)
            publishToNuget(version)
        else
            // other, unknown tag
            justBuild()
    else
        // no tag, just build
        justBuild()
)

Target.runOrDefault "BuildRelease"

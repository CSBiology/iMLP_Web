open System.IO
open System
open System.Reflection
open System.Net

open Shared

open Suave
open Suave.Files
open Suave.Successful
open Suave.Filters
open Suave.Operators

open Fable.Remoting.Server
open Fable.Remoting.Suave


open System.IO
open System.Net
open System.Threading

open Suave
open Suave.Files
open Suave.Successful
open Suave.Filters
open Suave.Operators

open Shared

open Fable.Remoting.Server
open Fable.Remoting.Suave

open BioFSharp.IO
open FSharpAux
open FSharpAux.IO
open FSharp.Plotly

open BioFSharp.BioTools
open FSharpAux
open Suave.Logging
open Suave.RequestErrors


module ServerPath =
    let workingDirectory =
        let currentAsm = Assembly.GetExecutingAssembly()
        let codeBaseLoc = currentAsm.CodeBase
        let localPath = Uri(codeBaseLoc).LocalPath
        Directory.GetParent(localPath).FullName

    let resolve segments =
        let paths = Array.concat [| [| workingDirectory |]; Array.ofList segments |]
        Path.GetFullPath(Path.Combine(paths))

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x
let publicPath = ServerPath.resolve [".."; "Client"; "public"]
let port = tryGetEnv "HTTP_PLATFORM_PORT" |> Option.map System.UInt16.Parse |> Option.defaultValue 8085us


let loggingOptions =
  { Literate.LiterateOptions.create() with
      getLogLevelText = function Verbose->"V" | Debug->"D" | Info->"I" | Warn->"W" | Error->"E" | Fatal->"F" }

let logger = LiterateConsoleTarget(
                name = [|"Suave";"Examples";"Example"|],
                minLevel = Verbose,
                options = loggingOptions,
                outputTemplate = "[{level}] {timestampUtc:o} {message} [{source}]{exceptions}"
              ) :> Logger


let config =
    { defaultConfig with
        homeFolder = Some publicPath
        bindings = [ HttpBinding.create HTTP (IPAddress.Parse "0.0.0.0") port ] 
        maxContentLength = 1000000000
        SuaveConfig.listenTimeout = new System.TimeSpan(0,0,5,0)
        logger = logger
        SuaveConfig.bufferSize = 1048576
        }

let plotFromScores (index:int) (scores: float array) = 
    let vals = 
        scores
        |> Array.mapi (fun i x -> (i+1,x)) 
    let plot =
        Chart.Column(vals)
        |> Chart.withY_AxisStyle("Score")
        |> Chart.withX_AxisStyle("Index of AminoAcid")
        |> Chart.withTitle(string index)
        |> Chart.withSize(600.,400.) 
    printfn "%A" plot
    GenericChart.toEmbeddedHTML plot

let rand = new System.Random()

let targetPResultsToCsv (res: seq<TargetPResult>) (id : System.Guid) =
    let str = 
        [
            yield ("Header","Sequence","Scores")
            for r in res do yield (r.Header,r.Sequence,r.Scores |> Array.fold (fun acc elem -> sprintf "%s; %f" acc elem) "")
        ]
        |> Seq.toCSV "\t" false
    printfn "%A" str 
    str
    |> Seq.write (sprintf "../Client/public/CsvResults/%s.csv" (id.ToString()))


let singleSequenceToMany (fsa:FastA.FastaItem<seq<char>>) =
    let header = fsa.Header
    let sequence = fsa.Sequence
    let sequences =
        let len = Seq.length fsa.Sequence
        [for i = 0 to len-1 do
            yield (sprintf ">%s" header)
            yield (Seq.skip i sequence) |> Array.ofSeq |> String.fromCharArray              
                ]
    sequences
    |> String.concat "\n"    


let targetPApi = {
    SingleSequenceRequest = 
        fun model single -> 
            async {
                    let targetModel = 
                        match model with
                        |Plant      -> TargetP.Plant
                        |NonPlant   -> TargetP.NonPlant
                        |_          -> failwithf "No model for targetP provided"
                    printfn "starting bc context"
                    let bcContext =
                        BioContainer.initBcContextLocalDefaultAsync TargetP.ImageTagetP
                        |> Async.RunSynchronously
                    printfn "bc context running"
                    let fastA = 
                        single
                        |> fun x -> x.Replace("\r\n","\n")
                        |> String.split '\n'
                        |> FastA.fromFileEnumerator id
                        |> Array.ofSeq
                    printfn "Fasta read"
                    let header = fastA.[0].Header
                    let sequences = fastA.[0] |> singleSequenceToMany
                    let byteArray = System.Text.Encoding.UTF8.GetBytes(sequences)
                    use sequenceStream = new MemoryStream(byteArray) 
                    printfn "inputSequences: \n %A" sequences 
                    printfn "starting targetP task"
                    let scores = 
                        (TargetP.run bcContext targetModel sequenceStream)
                        |> Seq.map (fun x -> x.Mtp)
                        |> Array.ofSeq
                    printfn "targetP done"
                    let plot = plotFromScores 1 scores
                    return {   
                            Header      =   header
                            Sequence    =   new System.String (fastA.[0].Sequence |> Array.ofSeq)
                            Scores      =   scores
                            PlotHtml    =   plot
                            }
            }
    FastaFileRequest = 
        fun model file -> 
            async {
                    let targetModel = 
                        match model with
                        |Plant      -> TargetP.Plant
                        |NonPlant   -> TargetP.NonPlant
                        |_          -> failwithf "No model for targetP provided"
                    printfn "starting bc context"
                    let bcContext =
                        BioContainer.initBcContextLocalDefaultAsync TargetP.ImageTagetP
                        |> Async.RunSynchronously
                    printfn "bc context running"
                    let fastA = 
                        file
                        |> fun x -> x.Replace("\r\n","\n")
                        |> String.split '\n'
                        |> FastA.fromFileEnumerator id
                        |> Array.ofSeq
                    let results =
                        fastA
                        |> Array.mapi (fun i fastaItem -> 
                                        let header = fastaItem.Header
                                        let sequences = fastaItem |> singleSequenceToMany
                                        let byteArray = System.Text.Encoding.UTF8.GetBytes(sequences)
                                        use sequenceStream = new MemoryStream(byteArray) 
                                        //printfn "inputSequences: \n %A" sequences 
                                        printfn "starting targetP task"
                                        let scores = 
                                            try 
                                                (TargetP.run bcContext targetModel sequenceStream)
                                                |> Seq.map (fun x -> x.Mtp)
                                                |> Array.ofSeq
                                            with e as exn ->    printfn "%s" e.Message
                                                                [||]
                                        let plot = plotFromScores i scores
                                        {   
                                            Header      =   header
                                            Sequence    =   new System.String (fastaItem.Sequence |> Array.ofSeq)
                                            Scores      =   scores
                                            PlotHtml    =   plot
                                        })
                    printfn "targetP done."
                    return results
                    }
    DownloadRequestSingle = 
        fun (res,id)-> 
            async { 
                    for f in (System.IO.Directory.EnumerateFiles("../Client/public/CsvResults/"))
                        do File.Delete(f)
                    targetPResultsToCsv [res] id
                    return ()
                    }

    DownloadRequestMultiple = 
        fun (res,id) -> 
            async {
                    for f in (System.IO.Directory.EnumerateFiles("../Client/public/CsvResults/"))
                        do File.Delete(f)
                    targetPResultsToCsv res id
                    return ()
                    }
}

open System

type CustomError = { errorMsg: string }

let errorHandler (ex: Exception) (routeInfo: RouteInfo<HttpContext>) : ErrorResult = 
    // do some logging
    printfn "Error at %s on method %s" routeInfo.path routeInfo.methodName
    // decide whether or not you want to propagate the error to the client
    match ex with
    | _ ->  Propagate {errorMsg = (sprintf "%s" ex.Message)}

let webApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.withDiagnosticsLogger (fun x -> if x.Length < 10000 then printfn "%s" x else (printfn "omitting some of the serialized result [length is above 10000 characters]...\r\n%s" x.[0..10000]))
    |> Remoting.withErrorHandler errorHandler
    |> Remoting.fromValue targetPApi
    |> Remoting.buildWebPart

let webApp =
    choose [
        webApi
        path "/" >=> browseFileHome "index.html"
        browseHome
        RequestErrors.NOT_FOUND "Not found!"
    ]

startWebServer config webApp


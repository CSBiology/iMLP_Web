open System.IO
open System
open System.Reflection
open System.Net

open Shared
open TargetPServer

open Suave
open Suave.Files
open Suave.Filters
open Suave.Operators

open Fable.Remoting.Server
open Fable.Remoting.Suave


open BioFSharp.IO
open FSharpAux
open FSharpAux.IO
open FSharp.Plotly

open BioFSharp.BioTools
open FSharpAux
open Suave.Logging


module Propensity =
    open FSharp.Stats

    let n = 3 // !!!
    
    let ofWindowed n (source:float[]) =
       if n < 0 then invalidArg "n" "n must be a positive integer"
       let lastIndex = source.Length - 1
       let arrSize = n + n + 1 |> float
    
       let globalMean = 0.1882641482
       let globalVar  = 0.1241744172
    
       let pfArr =
           source
           |> FSharp.Stats.Signal.Filtering.savitzky_golay (if source.Length < 21 then 3 else 21) 1 0 0
           |> Seq.toArray
    
       Array.init source.Length
           (fun i ->
               match i with
               | pos when pos < n ->
                   Array.foldSub (+) 0.0 pfArr 0 (pos+n) / float (pos+n+1)
               | pos when pos+n > lastIndex  ->
                   Array.foldSub (+) 0.0 pfArr (pos-n) lastIndex / float (source.Length-pos+n)
               | _ ->
                   Array.foldSub (+) 0.0 pfArr (i-n) (i+n) / arrSize
           )
       |> Array.map (fun x -> (x - globalMean) / globalVar )


module PlotHelpers =
    let xAxis title =
        Axis.LinearAxis.init
            (
                Title=title,
                Showgrid=false,
                Showline=true,
                Mirror=StyleParam.Mirror.All,
                Zeroline=false,
                Tickmode=StyleParam.TickMode.Auto,
                Ticks= StyleParam.TickOptions.Inside,
                Tickfont=Font.init(StyleParam.FontFamily.Arial,Size=18),
                Titlefont=Font.init(StyleParam.FontFamily.Arial,Size=18)
            )

    let yAxis title =
        Axis.LinearAxis.init
            (
                Title=title,
                Showgrid=false,
                Showline=true,
                Mirror=StyleParam.Mirror.All,
                Tickmode=StyleParam.TickMode.Auto,
                Ticks= StyleParam.TickOptions.Inside,
                Tickfont=Font.init(StyleParam.FontFamily.Arial,Size=18.),
                Titlefont=Font.init(StyleParam.FontFamily.Arial,Size=18.)
            )

    let csbDarkBlue = FSharpAux.Colors.fromRgb 68 84 106

    let csbOrange = FSharpAux.Colors.fromRgb 237 125 49

    let plotFromScores (index:int) (scores: float array) = 
        let vals = 
            scores
            |> Array.mapi (fun i x -> (i+1,x)) 
        let plot =
            [
                Chart.Spline(vals,Color = FSharpAux.Colors.toWebColor csbOrange,Name = "Smoothed")
                Chart.Column(vals,Color = FSharpAux.Colors.toWebColor csbDarkBlue,Name = "Scores")
            
            ]
            |> Chart.Combine
            |> Chart.withY_Axis(xAxis "Score")
            |> Chart.withX_Axis(yAxis "Index of AminoAcid")
            |> Chart.withTitle("iMTS-L propensity")
            |> Chart.withLayout(Layout.init(Paper_bgcolor="rgba(0,0,0,0)",Plot_bgcolor="white"))
            |> Chart.withSize(600.,600.)

        GenericChart.toEmbeddedHTML plot

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
    let len = Seq.length fsa.Sequence
    [for i = 0 to len-1 do
        yield
            FastA.createFastaItem (sprintf ">%s" header) (Seq.skip i sequence) 
            ]

let targetPApi = {
    SingleSequenceRequest = 
        fun model single -> 
            async {

                //set up parameters and Biocontainer + context
                let targetModel = 
                    match model with
                    |Plant      -> TargetP.Plant
                    |NonPlant   -> TargetP.NonPlant
                    |_          -> failwithf "No model for targetP provided"

                let client = Docker.connect "npipe://./pipe/docker_engine"
                let tpContext = 
                        BioContainer.initBcContextWithMountAsync client TargetP.ImageTagetP @"C:\Users\Kevin\source\repos\TargetPService\src\Server\tmp"
                        |> Async.RunSynchronously

                //read fasta item from input
                let fastA =
                    if single.StartsWith(">") then
                        single
                        |> fun x -> x.Replace("\r\n","\n")
                        |> String.split '\n'
                        |> FastA.fromFileEnumerator id
                        |> Array.ofSeq
                    else
                        sprintf ">No Header Provided\n%s" single
                        |> fun x -> x.Replace("\r\n","\n")
                        |> String.split '\n'
                        |> FastA.fromFileEnumerator id
                        |> Array.ofSeq

                //Save fasta to temporary container path
                let tmpPath = (sprintf @"C:\Users\Kevin\source\repos\TargetPService\src\Server\tmp\%s.fsa" (System.Guid.NewGuid().ToString()))
                fastA.[0]
                |> fun x -> {x with Sequence = x.Sequence |> Seq.filter (fun aa -> not (aa = '*' || aa = '-' ))}
                |> singleSequenceToMany
                |> FastA.write id tmpPath

                //Run Biocontainer
                let header = fastA.[0].Header
                let scores = 
                    TargetPServer.runWithMount tpContext targetModel tmpPath
                    |> Seq.map (fun x -> x.Mtp)
                    |> Array.ofSeq

                //Cleanup
                //dispose running container
                BioContainer.disposeAsync tpContext
                |> Async.RunSynchronously
                //delete temporary file
                File.Delete(tmpPath)

                //return result
                let propensity = Propensity.ofWindowed 3 scores
                let propensityPlot = PlotHelpers.plotFromScores 1 propensity

                return {   
                        Header      =   header
                        Sequence    =   new System.String (fastA.[0].Sequence |> Seq.filter (fun aa -> not (aa = '*' || aa = '-' )) |> Array.ofSeq)
                        Scores      =   scores
                        Propensity  =   propensity
                        PlotHtml    =   propensityPlot
                        }
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
    | _ ->  Propagate {errorMsg = (sprintf "%s \r\n %A" ex.Message ex.StackTrace)}

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


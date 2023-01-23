open System.IO
open System
open System.Reflection
open System.Net

open Shared
open TargetPServer
open CNTKServer
open InputSanitizing

open Suave
open Suave.Files
open Suave.Filters
open Suave.Operators

open Fable.Remoting.Server
open Fable.Remoting.Suave

open BioFSharp
open BioFSharp.IO
open FSharpAux
open FSharpAux.IO
open FSharp.Plotly

open BioFSharp.BioContainers
open FSharpAux
open Suave.Logging

module Config =

    type DeployMode =
    |Local
    |Server
    |Docker


    let deployConfig =
        DeployMode.Docker

module Paths =

    open Config

    [<Literal>]
    let local = @"C:\Users\schne\source\repos\kMutagene\TargetPService\src"

    [<Literal>]
    let server = @"C:\SafeApps\TargetPService\deploy"

    let getLocalPath p =
        Path.Combine(local,p)

    let getAbsoluteServerPath p =
        Path.Combine(server,p)

    let deploymentSpecificPath =
        match deployConfig with
        |Local -> getLocalPath
        |Server -> getAbsoluteServerPath
        |Docker -> fun p -> Path.Combine("",p)

/// ensure directory is created
let _ = Directory.CreateDirectory (Paths.deploymentSpecificPath "Client/public/CsvResults")

module Propensity =
    open FSharp.Stats

    let n = 3 // !!!
    
    let ofWindowed n (source:float[]) =

        if n < 0 then invalidArg "n" "n must be a positive integer"

        let lastIndex = source.Length - 1

        let arrSize = n + n + 1 |> float

        //Normalization to make sequences of different lengths comparable
        //expected value for the iMTS-L score over n=5000 random sequences generated by using the yeast amino acid frequency
        let globalMean = 0.1882641482
        //variance for the iMTS-L score over n=5000 random sequences generated by using the yeast amino acid frequency
        let globalVar  = 0.1241744172

        //Savitky-golay smoothed input
        //windowsize 21 is the expected length of MTS
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

    let smoothOnly n (source:float[]) =

        if n < 0 then invalidArg "n" "n must be a positive integer"

        let lastIndex = source.Length - 1

        let arrSize = n + n + 1 |> float

        //Savitky-golay smoothed input
        //windowsize 21 is the expected length of MTS
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

module PlotHelpers =

    type PlotMode =
    |Propensity
    |TargetPScore

    let config =
        Config.init (
            Responsive = true,
            ToImageButtonOptions = ToImageButtonOptions.init(
                StyleParam.ImageFormat.SVG
            )
        )

    let xAxis title (zeroline : bool)=
        Axis.LinearAxis.init
            (
                Title=title,
                Showgrid=false,
                Showline=true,
                Mirror=StyleParam.Mirror.All,
                Zeroline=zeroline,
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

    let insideLegend () =
        Legend.init(
            fun l ->
                l?x <- 0.02
                l?y <- 0.98
                l?traceorder <- "normal"
                l?bgcolor <- "rgba(222, 235, 247, 0.6)"
                l?bordercolor <- "rgb(68, 84, 106)"
                l?borderwidth <- "2"
                l
        )

    let layout = fun () ->
        let la = 
            Layout.init(Paper_bgcolor="rgba(0,0,0,0)",Plot_bgcolor="white")
        la?legend <- insideLegend ()
        la

    let csbDarkBlue = FSharpAux.Colors.fromRgb 68 84 106

    let csbOrange = FSharpAux.Colors.fromRgb 237 125 49

    let plotPropensity (name:string) (propensityScores: float array) =

        let vals = propensityScores |> Array.mapi (fun i x -> (i+1,x))

        Chart.SplineArea(vals,Color = "rgb(237, 125, 49, 0.9)",Name = "Propensity Score",Width = 2.5)
        |> Chart.withY_Axis(xAxis "Score" true)
        |> Chart.withX_Axis(yAxis "Index of AminoAcid")
        |> Chart.withTitle(sprintf "%s" name)
        |> Chart.withLayout(Layout.init(Paper_bgcolor="rgba(0,0,0,0)",Plot_bgcolor="white"))
        |> Chart.withSize(600.,600.)
        |> Chart.withConfig config
        |> GenericChart.toEmbeddedHTML

    let plotRawTargetPScores (name:string) (rawScores: float array) =

        let vals = rawScores |> Array.mapi (fun i x -> (i+1,x))

        let smoothed =
            Propensity.smoothOnly 3 rawScores
            |> Array.mapi (fun i x -> (i+1,x)) 
        [
            Chart.Spline(smoothed,Color = FSharpAux.Colors.toWebColor csbOrange,Name = "smoothed",Width = 2.5)
            Chart.Column(vals,Color = "rgba(68, 84, 106, 0.85)",Name = "raw TargetP score")
            
        ]
        |> Chart.Combine
        |> Chart.withY_Axis(xAxis "Score" false)
        |> Chart.withY_AxisStyle("Score",MinMax=(0.,1.2))
        |> Chart.withX_Axis(yAxis "Index of AminoAcid")
        |> Chart.withTitle(sprintf "%s" name)
        |> Chart.withLayout (layout ())
        |> Chart.withSize(600.,600.)
        |> Chart.withConfig config
        |> GenericChart.toEmbeddedHTML

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

let deleteTempFiles () = 
    let path = Paths.deploymentSpecificPath @"Client/public/CsvResults"
    for f in (System.IO.DirectoryInfo(path).GetFiles())
        do if (System.DateTime.Now.Subtract( f.CreationTime)).Minutes > 1 then File.Delete(f.FullName)

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

let legacyResultsToCsv (res: seq<LegacyResult>) (id : System.Guid) =
    let str = 
        [
            yield ("Header","Sequence","Raw_TargetP_Scores","iMTS-L_Propensity_Scores")
            for r in res do
                yield
                    (
                        r.Header,
                        r.Sequence,
                        r.RawTargetPScores |> Array.fold (fun acc elem -> if acc = "" then string elem else sprintf "%s; %f" acc elem) "",
                        r.PropensityScores |> Array.fold (fun acc elem -> if acc = "" then string elem else sprintf "%s; %f" acc elem) ""
                    )
        ]
        |> Seq.toCSV "\t" false
    str
    |> Seq.write
        (
            sprintf @"Client/public/CsvResults/%s.txt" (id.ToString())
            |> Paths.deploymentSpecificPath
        )

let iMLPResultsToCsv (res: seq<IMLPResult>) (id : System.Guid) =
    let str = 
        [
            yield ("Header","Sequence","iMTS-L_Propensity_Scores")
            for r in res do
                yield
                    (
                        r.Header,
                        r.Sequence,
                        r.PropensityScores |> Array.fold (fun acc elem -> if acc = "" then string elem else sprintf "%s; %f" acc elem) ""
                    )
        ]
        |> Seq.toCSV "\t" false
    str
    |> Seq.write
        (
            sprintf @"Client/public/CsvResults/%s.txt" (id.ToString())
            |> Paths.deploymentSpecificPath
        )


let singleSequenceToMany (fsa:FastA.FastaItem<BioArray.BioArray<AminoAcids.AminoAcid>>) =
    let header = fsa.Header
    let sequence = fsa.Sequence
    let len = Seq.length fsa.Sequence
    [for i = 0 to len-1 do
        yield
            FastA.createFastaItem (sprintf ">%s" header) (Seq.skip i sequence) 
            ]
    //targetP fails on Sequences longer than 1200 amino acids, so be safe and split sequences in 1000 item sized chunks
    |> Seq.chunkBySize 1000


let targetPApi = {
    getVersion = fun () -> async { return "0.0.1" }

    SingleSequenceRequestLegacy = fun (model,single) -> async {
        try
            let header, rawSequence =
                extractHeaderAndSequence single

            let joinedRawSequence = rawSequence |> String.concat "" 

            let sanitizedInput =  sanitizeInputSequence joinedRawSequence

            match sanitizedInput with
            | EmptySequence | FilteredEmptySequence | InvalidCharacters _ | InternalServerError ->

                return
                    LegacyResult.create
                        header
                        joinedRawSequence
                        [||]
                        [||]
                        [||]
                        ""
                        ""
                        sanitizedInput

            | ShortSequence sanitized
            | FilteredShortSequence sanitized
            | ContainsGapTerOJ sanitized
            | Success sanitized ->
                //Success of stringread fasta item from input
                let targetModel =
                    match model with
                    | Plant     -> TargetP.Plant
                    | NonPlant  -> TargetP.NonPlant

                let client = Docker.connect "npipe://./pipe/docker_engine"
                let tpContext = 
                        BioContainer.initBcContextWithMountAsync
                            client
                            (Docker.ImageId "targetp:1.1")
                            (
                                @"Server\tmp"
                                |> Paths.deploymentSpecificPath
                            )
                        |> Async.RunSynchronously

                //Save fasta to temporary container path

                let splitSeqs = 
                    FastA.createFastaItem
                        header
                        (BioArray.ofAminoAcidString sanitized)
                    |> singleSequenceToMany

                let paths =
                    splitSeqs
                    |> Seq.map
                        (fun _ ->
                            System.Threading.Thread.Sleep 10
                            sprintf @"Server\tmp\%s.fsa" (System.Guid.NewGuid().ToString())
                            |> Paths.deploymentSpecificPath
                        )
                    |> Array.ofSeq
                    
                splitSeqs
                |> Seq.iter2 (fun tmpPath tpreq -> FastA.write AminoAcids.symbol tmpPath tpreq) paths

                //Run Biocontainer
                let scores = 
                    paths
                    |> Array.map (fun tmpPath -> TargetPServer.runWithMount tpContext targetModel tmpPath)
                    |> Array.map (fun (tpres) -> tpres |>  Seq.map (fun x -> x.Mtp))
                    |> Seq.concat
                    |> Array.ofSeq

                let smoothed = Propensity.smoothOnly 3 scores

                //Cleanup
                //dispose running container
                BioContainer.disposeAsync tpContext
                |> Async.RunSynchronously
                //delete temporary file
                paths |> Array.iter File.Delete

                //return result
                let scorePlot = PlotHelpers.plotRawTargetPScores "Raw TargetP Scores" scores
                let propensity = Propensity.ofWindowed 3 scores
                let propensityPlot = PlotHelpers.plotPropensity "iMTS-L propensity" propensity

                return
                    LegacyResult.create
                        header
                        sanitized
                        scores
                        smoothed
                        propensity
                        propensityPlot
                        scorePlot
                        sanitizedInput

        with _ ->
            return
                LegacyResult.create
                    ""
                    ""
                    [||]
                    [||]
                    [||]
                    ""
                    ""
                    InternalServerError
    }

    SingleSequenceRequestIMLP =
        fun (model,single) -> async {
            try 
                let header, rawSequence =
                    extractHeaderAndSequence single

                let joinedRawSequence = rawSequence |> String.concat "" 

                let sanitizedInput =  sanitizeInputSequence joinedRawSequence

                match sanitizedInput with
                | EmptySequence | FilteredEmptySequence | InvalidCharacters _ | InternalServerError ->
                    return
                        IMLPResult.create
                            header
                            joinedRawSequence
                            [||]
                            ""
                            sanitizedInput

                | ShortSequence sanitized
                | FilteredShortSequence sanitized
                | ContainsGapTerOJ sanitized
                | Success sanitized ->

                    printfn "sanitized input Length : %i" sanitized.Length

                    let imlpPropensity =
                        sanitized
                        |> CNTKServer.Prediction.predictIMTSLPropensityForSequence (CNTKServer.Models.getModelBuffer model)

                    printfn "propensitiy profile Length : %i" imlpPropensity.Length

                    return
                        IMLPResult.create
                            header
                            sanitized
                            imlpPropensity
                            (PlotHelpers.plotPropensity "iMTS-L propensity" imlpPropensity)
                            sanitizedInput
            with e ->
                return
                    IMLPResult.create
                        e.Message
                        (e.ToString())
                        [||]
                        ""
                        InternalServerError

        }

    DownloadRequestSingleLegacy = fun (res,id) -> async {
        deleteTempFiles()
        legacyResultsToCsv [res] id
        return ()
    }

    DownloadRequestSingleIMLP = fun (res,id) -> async { 
        deleteTempFiles()
        iMLPResultsToCsv [res] id
        return ()
    }

    DownloadRequestMultipleLegacy = fun (res,id) -> async {
        deleteTempFiles()
        legacyResultsToCsv res id
        return ()
    }

    DownloadRequestMultipleIMLP = fun (res,id) -> async {
        deleteTempFiles()
        iMLPResultsToCsv res id
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
    | _ ->  Propagate ex

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
        //pathScan "/api/csvresults/%s" (fun fileName -> file (ServerPath.resolve ["..";"..";"client";"public";fileName]))
        browseHome
        RequestErrors.NOT_FOUND "Not found!"
    ]

startWebServer config webApp


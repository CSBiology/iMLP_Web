module Client

open System
open System.IO
open System.Text

open Elmish
open Elmish.React

//open Fable.Fetch
open Thoth.Json

open Fable.React.Props
open Fable.Core.JsInterop
open Shared
open Fulma
//open Fulma.FontAwesome
open Fable.React
open Browser.Dom
open Browser.Types
open Fable.FontAwesome

let plotlyTestChart ="""<div id="8eb33214-037b-4012-ae59-2f74d109816f" style="width: 600px; height: 600px;"><!-- Plotly chart will be drawn inside this DIV --></div> """

let plotlyTestChartData = """
    var data = [{"type":"bar","x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"y":[51,45,45,45,37,34,30,27,20,19,18,17,17,15,15,12,11,8,8,7,6,6,5,5,4,3,3,1,1,1,1],"marker":{},"name":"CoffeeConsumption","xaxis":"x","yaxis":"y"},{"type":"bar","x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],"y":[15,11,7,6,4,3,3,3,3,3,2,2,2,1,1,1,1],"marker":{},"name":"BeerConsumption","xaxis":"x2","yaxis":"y2"},{"type":"bar","x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23],"y":[15,9,9,9,8,5,4,4,3,3,3,3,2,2,2,2,2,1,1,1,1,1,1,1],"marker":{},"name":"BeverageConsumption","xaxis":"x3","yaxis":"y3"}];
    var layout = {"yaxis":{"tickmode":"auto","ticks":"inside","mirror":"all","showline":true,"showgrid":false,"titlefont":{"family":"Arial","size":15.0},"tickfont":{"family":"Arial","size":15.0},"title":"Consumption [cups]","anchor":"x","domain":[0.71666666666666667,1.0]},"xaxis":{"tickmode":"auto","ticks":"inside","mirror":"all","showline":true,"showgrid":false,"zeroline":false,"titlefont":{"family":"Arial","size":15.0},"tickfont":{"family":"Arial","size":15.0},"anchor":"y","domain":[0.0,0.95]},"yaxis2":{"tickmode":"auto","ticks":"inside","mirror":"all","showline":true,"showgrid":false,"titlefont":{"family":"Arial","size":15.0},"tickfont":{"family":"Arial","size":15.0},"title":"Consumption [huelsen]","anchor":"x2","domain":[0.38333333333333341,0.66666666666666674]},"xaxis2":{"tickmode":"auto","ticks":"inside","mirror":"all","showline":true,"showgrid":false,"zeroline":false,"titlefont":{"family":"Arial","size":15.0},"tickfont":{"family":"Arial","size":15.0},"anchor":"y2","domain":[0.0,0.95]},"yaxis3":{"tickmode":"auto","ticks":"inside","mirror":"all","showline":true,"showgrid":false,"titlefont":{"family":"Arial","size":15.0},"tickfont":{"family":"Arial","size":15.0},"title":"Consumption [bottles]","anchor":"x3","domain":[0.050000000000000044,0.33333333333333337]},"xaxis3":{"tickmode":"auto","ticks":"inside","mirror":"all","showline":true,"showgrid":false,"zeroline":false,"titlefont":{"family":"Arial","size":15.0},"tickfont":{"family":"Arial","size":15.0},"anchor":"y3","domain":[0.0,0.95]},"width":1000.0,"height":750.0};
    Plotly.newPlot('8eb33214-037b-4012-ae59-2f74d109816f', data, layout);"""

module FileInputHelper =

    //!!===================================================!!
    //Credit goes to https://github.com/GuuD/fable-file-input
    //!!===================================================!!

    open Fable.Core.DynamicExtensions
    open Fable.Core
    open Browser.Dom
    open Browser.Types
    type FileInfo<'t> = 
        { Name: string; MIME: string; Data: 't }

    [<Emit("new Promise($0)")>]
    let createPromise (executor: ('t -> unit) -> (exn -> unit) -> unit): JS.Promise<'t> = jsNative

    [<Emit("$1.then($0)")>]
    let consumePromise (callback: 't->unit) (_promise: JS.Promise<'t>): unit = jsNative
    let private readInternal<'t> (readMethod: string) (blob: Browser.Types.Blob) = 
        createPromise(fun resolve reject -> 
            try
                let reader = Browser.Dom.FileReader.Create()
                reader.onload <- (fun _ -> reader.result |> unbox<'t> |> resolve)
                reader.[readMethod].Invoke(blob) |> ignore
            with 
            | e -> e |> reject
        )
    let readAsText blob: JS.Promise<string> = 
        readInternal "readAsText" blob
    let readAsDataURL blob: JS.Promise<string> = 
        readInternal "readAsDataURL" blob
    let readAsArrayBuffer blob: JS.Promise<JS.ArrayBuffer> = 
        readInternal "readAsArrayBuffer" blob
    module React = 
        open Fable.React
        open Props
        let extract f list = 
            let rec seek traversed = function
                | h::t ->
                    match f h with
                    | Some h' -> Some h', (List.rev traversed)@t
                    | _ -> seek (h::traversed) t
                | [] -> None, (List.rev traversed)
            seek [] list

        type FileCallback = 
            | OnFileBytesReceived of (FileInfo<JS.ArrayBuffer> -> unit)
            | OnDataUrlReceived of (FileInfo<string> -> unit)
            | OnTextReceived of (FileInfo<string> -> unit)
            interface Props.IHTMLProp

        let inline singleFileInput (props: Props.IHTMLProp list) = 
            let existingChangeHandler, otherProps = 
                props |> extract (function 
                    | :? DOMAttr as prop -> 
                        match prop with
                        | OnChange callback -> Some callback
                        | _ -> None
                    | _ -> None)
            let loadCallback, withoutCallbacks = otherProps |> extract (function | :? FileCallback as fc -> Some fc | _ -> None)   
            let changeHandler (e : Event) = 
                let files: FileList = !!e.target.["files"]
                if files.length > 0 then
                    let file = files.[0]
                    match loadCallback with
                    | Some(OnFileBytesReceived r) -> 
                        readAsArrayBuffer file
                        |> consumePromise (fun bytes -> r { Name = file.name; MIME = file.``type``; Data = bytes })
                    | Some(OnDataUrlReceived r) ->
                        readAsDataURL file
                        |> consumePromise (fun data -> r { Name = file.name; MIME = file.``type``; Data = data } )
                    | Some(OnTextReceived r) ->
                        readAsText file
                        |> consumePromise (fun data -> r { Name = file.name; MIME = file.``type``; Data = data } )
                    | _ -> console.warn("You probably need to attach callback to the file input field")                
                match existingChangeHandler with
                | Some h -> h e
                | _ -> ()
            input ([OnChange changeHandler; Type "file" ]@withoutCallbacks)

module Server =

    open Shared
    open Fable.Remoting.Client

    let targetPApi : ITargetPApi = 
        Remoting.createApi()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.buildProxy<ITargetPApi>

    /// A proxy you can use to talk to server directly
//    let api : ICounterApi =
//      Remoting.createApi()
//      |> Remoting.withRouteBuilder Route.builder
//      |> Remoting.buildProxy<ICounterApi>

let validateFastaText (fsa:string) =
    let allSeqs =
        fsa
        |> fun x -> x.Replace("\r\n","\n")
        |> fun x -> x.Split('\n')
        |> Array.filter (fun x -> not (x.StartsWith(">")))
    let validSet = set ['A';'C';'D';'E';'F';'G';'H';'I';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'Y';'X';'J';'Z';'B';'*';'-';'\n';'\r']

    let invalidChars =
        allSeqs
        |> Array.map (String.filter (fun x -> not (validSet.Contains(x))))
        |> Array.filter (fun s -> s.Length > 0)
        |> String.concat ""
        |> fun x -> [for c in x do yield c]
        |> List.distinct

    let isValid = invalidChars.Length = 0

    if isValid then Ok fsa else Error (invalidChars)

open FileInputHelper
open FileInputHelper.React
open System

type Mode =
|NotSelected
|Single
|File

type DisplayHelp =
|NoHelp
|TechnicalDetails
|Contact
|HowToUse
|Publication
|InputFormat

type PlotMode =
|Propensity
|TargetPScore

// The model holds data that you want to keep track of while the application is running
type Model = { 
    SessionGuid                 :   System.Guid
    BurgerVisible               :   bool
    SelectedTargetPModel        :   TargetPModel
    SingleSequence              :   string
    SingleSequenceResult        :   TargetPResult Option
    FastaFileInput              :   string []
    FastaFileInputName          :   string
    FastaFileInputResult        :   (TargetPResult array) Option
    SeqMode                     :   Mode
    ShowResults                 :   bool
    ResultHeadingIsSticky       :   bool
    CurrentResultViewIndex      :   int
    DownloadReady               :   bool
    DownloadFileName            :   string
    FileProcessIndex            :   int
    HasValidFasta               :   bool
    InvalidFastaChars           :   char list
    HasJobRunning               :   bool
    ShowProgressDetails         :   bool
    InformationSectionDisplay   :   DisplayHelp
    HasError                    :   bool
    ErrorState                  :   exn Option
    PlotMode                    :   PlotMode
}

let initialModel = {
    SessionGuid                 =   System.Guid.NewGuid()
    BurgerVisible               =   false
    SelectedTargetPModel        =   TargetPModel.NonPlant
    SingleSequence              =   ""
    SingleSequenceResult        =   None
    FastaFileInput              =   [||]
    FastaFileInputName          =   "No file selected"
    FastaFileInputResult        =   None
    SeqMode                     =   Mode.NotSelected
    ShowResults                 =   false
    ResultHeadingIsSticky       =   false
    CurrentResultViewIndex      =   0
    DownloadReady               =   false
    DownloadFileName            =   "IMTS_prediction_results.tsv"
    FileProcessIndex            =   0
    HasValidFasta               =   true
    InvalidFastaChars           =   []
    HasJobRunning               =   false
    ShowProgressDetails         =   false
    InformationSectionDisplay   =   NoHelp
    HasError                    =   false
    ErrorState                  =   None
    PlotMode                    =   Propensity
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
| Reset
| ToggleBurger
| TargetPModelSelection     of TargetPModel
| SeqModeSelection          of Mode
| FastaUploadInput          of string*string
| SingleSequenceInput       of string
| SingleSequenceRequest
| SingleSequenceResponse    of Result<TargetPResult,exn>
| FastaUploadRequest
| FastaUploadResponse       of Result<TargetPResult,exn>
| FileProcessingDone
| ShowPlot                  of int 
| PrepareDownloadCSV
| DownloadResponse          of Result<unit,exn>
| DownloadFileNameChange    of string
| ShowProgressDetails
| ToggleResultHeadingSticky of bool
| ChangeViewIndex           of int
| ChangeHelpDisplay         of DisplayHelp
| FastaValidation           of Result<string,char list>
| GenericError              of exn
| ChangePlotMode            of PlotMode

let gradientColorTable = [|
    "#FAEE05"
    "#FAEE05"
    "#F8E109"
    "#F7D40E"
    "#F5C813"
    "#F4BB18"
    "#F2AF1D"
    "#F1A222"
    "#EF9627"
    "#EE892C"
    "#ED7D31"
|]

type CustomHTMLAttr = 
    | [<CompiledName("data-dismiss")>] DataDismiss of string
    | [<CompiledName("aria-label")>] AriaLabel of string
    | [<CompiledName("aria-hidden")>] AriaHidden of bool
    interface IHTMLProp 

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    initialModel, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | Reset -> init ()
    | GenericError exn ->
        let updatedModel = {currentModel with HasError = true; ErrorState=Some exn}
        updatedModel,Cmd.none
    | ChangePlotMode pm ->
        let updatedModel = {currentModel with PlotMode = pm}
        updatedModel,Cmd.none
    | ChangeHelpDisplay hd ->
        let updatedModel = {currentModel with InformationSectionDisplay = hd}
        updatedModel,Cmd.none
    | ChangeViewIndex i ->
        let newIndex = currentModel.CurrentResultViewIndex + i
        let updatedModel =
            if newIndex >= 0 && newIndex < currentModel.FileProcessIndex then
                {currentModel with CurrentResultViewIndex = currentModel.CurrentResultViewIndex + i}
            else
                currentModel
        updatedModel,Cmd.none
    | SeqModeSelection sm ->
        let updatedModel = {currentModel with SeqMode = sm}
        updatedModel,Cmd.none

    | TargetPModelSelection m ->
        let updatedModel = {currentModel with SelectedTargetPModel = m}
        updatedModel,Cmd.none

    | ToggleBurger -> 
        let updatedModel = {currentModel with BurgerVisible = not currentModel.BurgerVisible}
        updatedModel,Cmd.none

    | SingleSequenceInput s -> 
        let seqMode =
            if s.Length > 0 then 
                Mode.Single
            else
                Mode.NotSelected
        let updatedModel = {currentModel with SingleSequence = s; SeqMode = seqMode}
        let validateCmd =
            Cmd.OfFunc.perform
                validateFastaText
                s
                FastaValidation


        updatedModel, validateCmd

    | FastaValidation (Ok _) ->
        let updatedModel = {currentModel with HasValidFasta = true; InvalidFastaChars = []}
        updatedModel,Cmd.none

    | FastaValidation (Error invalidChars) ->
        let updatedModel = {currentModel with HasValidFasta = false; InvalidFastaChars = invalidChars}
        updatedModel,Cmd.none
        

    | FastaUploadInput (fileData,fileName) -> 
        let seqMode =
            if fileData.Length > 0 then 
                Mode.File
            else
                Mode.NotSelected
        let updatedModel = {
            currentModel with
                FastaFileInput =
                    fileData.Split('>')
                    |> fun x -> [|yield ([x.[0];x.[1]] |> String.concat ""); yield! x.[2 ..]|]
                    |> Array.map (sprintf ">%s")
                FastaFileInputName = fileName
                SeqMode = seqMode
            }
        let validateCmd =
            Cmd.OfFunc.perform
                validateFastaText
                fileData

                FastaValidation

        updatedModel,validateCmd
            

    | SingleSequenceRequest ->

        let updatedModel = {
            currentModel with
                DownloadReady = false;
                HasJobRunning = true
                }
        let requestCmd = 
            Cmd.OfAsync.either
                (Server.targetPApi.SingleSequenceRequest currentModel.SelectedTargetPModel)
                currentModel.SingleSequence
                (Ok >> SingleSequenceResponse)
                (Error >> SingleSequenceResponse)
        updatedModel,requestCmd

    | FastaUploadRequest ->

        let updatedModel = {
            currentModel with
                DownloadReady = false;
                HasJobRunning = true;
            }
        let fileLength = currentModel.FastaFileInput.Length
        let processIndex = currentModel.FileProcessIndex

        let requestCmd =
            if processIndex < fileLength then
                Cmd.OfAsync.either
                    (Server.targetPApi.SingleSequenceRequest currentModel.SelectedTargetPModel)
                    currentModel.FastaFileInput.[processIndex]
                    (Ok >> FastaUploadResponse)
                    (Error >> FastaUploadResponse)
            else
                Cmd.ofMsg FileProcessingDone
                    
        updatedModel,requestCmd

    | SingleSequenceResponse (Ok res) ->
        let updatedModel = {
            currentModel with 
                SingleSequenceResult = Some res
                ShowResults = true
                HasJobRunning = false
                }
        updatedModel,Cmd.none

    | SingleSequenceResponse (Error res) ->
        console.log(res)
        currentModel,Cmd.ofMsg (GenericError res)

    | FastaUploadResponse (Ok res) ->
        let updatedModel = {
            currentModel with 
                FastaFileInputResult =
                    match currentModel.FastaFileInputResult with
                    |Some cres  -> Some [|yield! cres; yield res|]
                    |None       -> Some [|res|]
                ShowResults = true
                FileProcessIndex = currentModel.FileProcessIndex + 1
                }
        updatedModel,Cmd.ofMsg FastaUploadRequest

    | FastaUploadResponse (Error res) ->
        //TODO: handle request error!
        console.log(res)
        currentModel,Cmd.ofMsg (GenericError res)

    | ToggleResultHeadingSticky state ->
        let updatedModel = {currentModel with ResultHeadingIsSticky = state}
        updatedModel,Cmd.none

    | ShowPlot index ->
        let updatedModel = {currentModel with CurrentResultViewIndex = index}
        updatedModel,Cmd.none

    | PrepareDownloadCSV ->
        match currentModel.SeqMode with
        |Single ->
            match currentModel.SingleSequenceResult with
            |Some res ->
                let requestCmd = 
                    Cmd.OfAsync.either
                        Server.targetPApi.DownloadRequestSingle
                        (res,currentModel.SessionGuid)
                        (Ok >> DownloadResponse)
                        (Error >> DownloadResponse)
                currentModel,requestCmd
            |_ -> currentModel,Cmd.none
        |File ->
            match currentModel.FastaFileInputResult with
                |Some res ->
                    let requestCmd = 
                        Cmd.OfAsync.either
                            Server.targetPApi.DownloadRequestMultiple
                            (res,currentModel.SessionGuid)
                            (Ok >> DownloadResponse)
                            (Error >> DownloadResponse)
                    currentModel,requestCmd
                |_ -> currentModel,Cmd.none
        |_ -> currentModel,Cmd.none

    | ShowProgressDetails ->
        let updatedModel = {currentModel with ShowProgressDetails = not currentModel.ShowProgressDetails}
        updatedModel, Cmd.none

    | FileProcessingDone ->
        let updatedModel = {currentModel with HasJobRunning = false}
        updatedModel, Cmd.none

    | DownloadResponse (Ok _) ->
        let updatedModel = {currentModel with DownloadReady = true}
        updatedModel,Cmd.none

    | DownloadResponse (Error ex) ->
        let updatedModel = {currentModel with DownloadReady = false}
        updatedModel,Cmd.ofMsg (GenericError ex)

    | DownloadFileNameChange dname ->
        let updatedModel = {currentModel with DownloadFileName = dname}
        updatedModel,Cmd.none

let navbar (model : Model) (dispatch : Msg -> unit) =
    let currentDisp = model.InformationSectionDisplay
    Navbar.navbar [Navbar.IsFixedTop; Navbar.CustomClass "is-dark csbNav"; Navbar.Props [Props.Role "navigation"; AriaLabel "main navigation" ]] [
        Navbar.Brand.a [] [
            Navbar.Item.a [Navbar.Item.Props [Props.Href "https://csb.bio.uni-kl.de/"]] [
                img [Props.Src "../Images/Logo.png"]
            ]
            Navbar.burger   [
                                Props [
                                    Class (if model.BurgerVisible then "navbar-burger is-active" else "navbar-burger")
                                    Props.Role "button"
                                    AriaLabel "menu"
                                    Props.AriaExpanded false
                                    OnClick (fun e -> ToggleBurger |> dispatch)
                                ]
                            ] [
                span [AriaHidden true] []
                span [AriaHidden true] []
                span [AriaHidden true] []
            ]
        ]
        Navbar.menu [Navbar.Menu.Props [Id "navbarMenu"; Class (if model.BurgerVisible then "navbar-menu is-active" else "navbar-menu") ]] [
            Navbar.Start.div [] [
                Navbar.Item.a
                    [
                        Navbar.Item.Props [OnClick (fun _ -> ChangeHelpDisplay (if currentDisp = HowToUse then NoHelp else HowToUse) |> dispatch)]
                        Navbar.Item.IsActive (currentDisp = HowToUse)
                    ] [
                    str "How to use"
                ]
                Navbar.Item.a
                    [
                        Navbar.Item.Props [OnClick (fun _ -> ChangeHelpDisplay (if currentDisp = Publication then NoHelp else Publication) |> dispatch)]
                        Navbar.Item.IsActive (currentDisp = Publication)
                    ] [
                    str "Publication"
                ]
                Navbar.Item.a
                    [
                        Navbar.Item.Props [OnClick (fun _ -> ChangeHelpDisplay (if currentDisp = InputFormat then NoHelp else InputFormat) |> dispatch)]
                        Navbar.Item.IsActive (currentDisp = InputFormat)
                    ] [
                    str "Input format"

                ]
                Navbar.Item.a
                    [
                        Navbar.Item.Props [OnClick (fun _ -> ChangeHelpDisplay (if currentDisp = TechnicalDetails then NoHelp else TechnicalDetails) |> dispatch)]
                        Navbar.Item.IsActive (currentDisp = TechnicalDetails)
                    ] [
                    str "Technical details"
                ]
            ]
            Navbar.End.div [] [
                Navbar.Item.a
                    [
                        Navbar.Item.Props [OnClick (fun _ -> ChangeHelpDisplay (if currentDisp = Contact then NoHelp else Contact) |> dispatch)]
                        Navbar.Item.IsActive (currentDisp = Contact)
                    ] [
                    str "Contact"
                ]
            ]
        ]
    ]

let getDisplayHelpText (model:Model) (dispatch:Msg->unit) =
    
    match model.InformationSectionDisplay with
    |NoHelp         -> []
    |TechnicalDetails ->
        [
            br []
            Heading.h4 [] [str "Technical Details   "; Icon.icon [Icon.Props [OnClick (fun _ -> ChangeHelpDisplay NoHelp |> dispatch)]] [Fa.i [Fa.Solid.Times] []]]
            br []
            str "This service uses a server-side nested virtualization procedure to enable querying a targetP docker container."
            br []
            br []
            str "Nested in this case means that the host machine, which itself is a windows server virtual machine running on a HyperV cluster acts itself as a host for the docker demon."
            br []
            str "This enables the realisation of OS-agnostic services, here the calling of targetP - a command line tool usually only available to unix host machines."
            br []
            br []
            str "We designed a F# library - BioFSharp.BioTools - to reliably run docker tasks from the .NET environment, therefore making dockerized non-windows applications available for the .NET ecosystem."
        ]
    |Contact        ->
        [
            br []
            Heading.h4 [] [str "Contact   "; Icon.icon [Icon.Props [OnClick (fun _ -> ChangeHelpDisplay NoHelp |> dispatch)]] [Fa.i [Fa.Solid.Times] []]]
            br []
            ul [] [
                li [] [a[] [str "Timo Mühlhaus"] ; str ", Computational Systems Biology Kaiserslautern"]
                li [] [a[] [str "Kevin Schneider"] ; str ", Computational Systems Biology Kaiserslautern"]
            ]
        ]
    |HowToUse       ->
        [
            br []
            Heading.h4 [] [str "How To Use   "; Icon.icon [Icon.Props [OnClick (fun _ -> ChangeHelpDisplay NoHelp |> dispatch)]] [Fa.i [Fa.Solid.Times] []]]
            br []
            ol [] [
                li [] [str "Some stepz"]
                li [] [str "Some stepz"]
                li [] [str "Some stepz"]
                li [] [str "Some stepz"]
            ]
        ]
    |Publication    ->
        [
            br []
            Heading.h4 [] [str "About the publication this service is built on   "; Icon.icon [Icon.Props [OnClick (fun _ -> ChangeHelpDisplay NoHelp |> dispatch)]] [Fa.i [Fa.Solid.Times] []]]
            br []
            str "Lorem ipsum dolor sit amet"
        ]
    |InputFormat    ->
        [
            br []
            Heading.h4 [] [str "Input format help   "; Icon.icon [Icon.Props [OnClick (fun _ -> ChangeHelpDisplay NoHelp |> dispatch)]] [Fa.i [Fa.Solid.Times] []]]
            br []
            str "The input for both single sequence or file mode has to be in fasta conform format. As this prediction algorithm predicts iMTS-L propensity of proteins, only protein sequences will produce valid output."
            br []
            str "Fasta conform means:"
            ul [] [
                li [] [str "each protein sequence is headed by a single line identifying header, started by the '>' character. In the case of a single sequence input the header can be omitted."]
                li [] [str "The sequence starts in the next line and only consist of valid amino acid characters (ACDEFGHIKLMNOPQRSTUVWY)"]
                li [] [str "Ambiguity characters (XJZB) are okay"]
                li [] [str "Gap and terminator characters (- and *) are filtered out by us. Just keep this in mind when you look at your profiles."]
                li [] [str "All other characters not mentioned above can lead to invalid output."]
            ]
        ]

let displayHelpSection (model:Model) (dispatch:Msg->unit) =
    Section.section
        [
            yield Section.CustomClass "HelpSection";
            if model.InformationSectionDisplay = NoHelp then
                yield Section.Props [Style [Display DisplayOptions.None]]
        ] [
        Container.container [] [
            Content.content [] (getDisplayHelpText model dispatch)
        ]
    ]

let pageinateFromIndex (resArray: TargetPResult array) (index:int) (model:Model) (dispatch:Msg->unit)  =
    let ofClass () = 
        if index = model.CurrentResultViewIndex then 
            Props.Class"pagination-link is-current"
        else 
            Props.Class "pagination-link"
    li [    ofClass ()
            Props.OnClick (fun _ -> ShowPlot (index) |> dispatch)
            ] [span [] [str (string (index+1))]]

let pageinateDynamic (pos: int) (res: TargetPResult array) (model:Model) (dispatch:Msg->unit) = 
    let indices =
        [0 .. res.Length-1]
    let numbers =
        let x = indices.[(max 1 (pos-2)) .. (min (pos+2) (res.Length-2)) ]
        printfn "%A" x
        x
        |> List.map (fun index -> pageinateFromIndex res index model dispatch) 
    numbers 

let fastaFormatDisplay (model:Model) (sequence:char array) (scores: float array) =
    console.log(sequence |> Seq.fold (fun a x -> sprintf "%s%c" a x) "")
    console.log(scores)
    let scores' =
        if model.PlotMode = Propensity then
            scores
            |> fun x ->
                let minVal = Array.min x
                x
                |> Array.map (fun x -> x + (abs minVal))
        else
            scores
    let maxVal = Array.max scores'
    let norm = 
        scores'
        |> Array.map (fun s -> s/maxVal)
        |> fun x ->
            console.log(sprintf "score length: %i" x.Length)
            console.log(sprintf "sequence length: %i" sequence.Length)
            x |> Array.map2 (fun char score -> int ((score) * 100.) ,char) sequence
    let spans =
        norm 
        |> Array.map (fun (score,char) -> span [Props.Style [
                                                                    CSSProp.BackgroundColor (gradientColorTable.[int (round (float score / 10.))]);
                                                                    CSSProp.Color "#44546A"
                                                                    ]] [b [] [str (string char)]])
    let formatStrings = 
        spans
        |> Array.chunkBySize 60
        |> Array.mapi (fun i spans ->   let lineIndex = (i * 60 + 1)
                                        let indexText = sprintf "%i%s" lineIndex ([for x in 1 .. (10 - (string lineIndex).Length) do yield "."] |> String.concat "")
                                        div [] [  
                                                    yield span [] [ str indexText]; 
                                                    yield! spans 
                                                    ])
    Content.content [] [
        yield! formatStrings 
    ]

let downloadBtn (location:string) (model:Model) (dispatch: Msg -> unit)=
    Columns.columns[] [
        Column.column [Column.Width (Screen.Desktop, Column.Is2)] [
            str "Enter filename:"
        ]
        Column.column [Column.Width (Screen.Desktop, Column.Is8)] [
            Input.text [
                        Input.Props []
                        Input.Placeholder model.DownloadFileName
                        Input.OnChange (fun e ->    let dname = !!e.target?value
                                                    DownloadFileNameChange dname |> dispatch)
                        ] 
        ]
        Column.column [Column.Width (Screen.Desktop, Column.Is2)] [
            a[
                Props.Download (  if ( model.DownloadFileName.EndsWith(".tsv")) then
                                        model.DownloadFileName
                                    else 
                                        sprintf "%s.tsv" model.DownloadFileName
                                        )
                Props.Href location
                Props.Class "is-primary is-full-width"
                ] [
                Icon.icon [] [Fa.i [Fa.Solid.Download] []]
                str "Click to download"
            ]
        ]
    ]

let downloadView (model:Model) (dispatch: Msg -> unit) =
    if model.DownloadReady then
        downloadBtn (sprintf "./CsvResults/%s.csv" (model.SessionGuid.ToString())) model dispatch
    else
        Button.button [
                    Button.IsLoading model.HasJobRunning
                    Button.Props [Props.Id "prepareDownload"]
                    Button.CustomClass "is-success" 
                    Button.IsFullWidth
                    Button.OnClick (fun _ -> PrepareDownloadCSV |> dispatch)] [
            str "Prepare Results as tab separated file for download"
        ]

let progressDetails (show:bool) (model : Model)  (dispatch: Msg -> unit)  =
    div [] [
        if model.HasJobRunning then 
            yield Button.button [Button.OnClick (fun _ -> ShowProgressDetails |> dispatch)] [if show then yield str "Hide progress details" else yield str "Show progress details"]
            yield
                Container.container [if (not show) then yield Container.Props [Props.Style [Props.Display DisplayOptions.None]]] [
                    Table.table [][
                        thead [] [
                            th [] [
                                str "Protein entry header"
                            ]
                            th [] [
                                str "Status"
                                ]
                        ]
                        tbody [] [
                           yield!
                            model.FastaFileInput
                            |> Array.mapi
                                (fun i entry ->
                                    let rowEntry =
                                        [
                                            td [] [
                                                str (entry.Split('\n') |> Array.head)
                                            ]
                                        ]
                                    if i < model.FileProcessIndex then
                                        tr [] [
                                            yield! rowEntry
                                            yield
                                                td [] [
                                                    str "Done"
                                                    Icon.icon [] [Fa.i [Fa.Solid.Check] []]
                                                ]
                                        ]
                                    elif i = model.FileProcessIndex then
                                        tr [] [
                                            yield! rowEntry
                                            yield
                                                td [] [
                                                    str "Processing"
                                                    Icon.icon [] [Fa.i [Fa.Solid.Spinner] []]
                                                ]
                                        ]
                                    else
                                        tr [] [
                                            yield! rowEntry
                                            yield
                                                td [] [
                                                    str "Queued"
                                                    Icon.icon [] [Fa.i [Fa.Solid.TruckLoading] []]
                                                ]
                                        ]
                                )
                        ]
                    ]
                ]
    ]

let progressView (show:bool) (model : Model)  (dispatch: Msg -> unit)  =
    let progr = (float model.FileProcessIndex / float model.FastaFileInput.Length)
    div [] [
        if model.HasJobRunning then 
            yield str (if progr < 1. then sprintf "Progress: %i/%i Proteins" model.FileProcessIndex model.FastaFileInput.Length else "Done.")
            yield progress [Class "progress is-link is-large"; Props.Value progr] [str (sprintf "%.2f%s" (float model.FileProcessIndex / float model.FastaFileInput.Length) "%")]
    ]

let resultBar (model : Model) (res: TargetPResult [] ) (dispatch : Msg -> unit) =
    let index = model.CurrentResultViewIndex
    div
        [
            yield Props.Class "resultBar"
            if (not (model.SeqMode = File && model.FastaFileInputResult.IsSome)) then
                yield Props.Style [Props.Display DisplayOptions.None]
        ] [
            Columns.columns [] [
                Column.column [Column.Width (Screen.Desktop, Column.Is7);Column.CustomClass "leftResultBar"] [
                    Heading.h3 [] [str "Navigate through results:"]
                ]
                Column.column [Column.Width (Screen.Desktop, Column.Is7);Column.CustomClass "rightResultBar"] [
                    if model.SeqMode = File && model.FastaFileInputResult.IsSome then
                        yield
                            nav [Class "pagination is-left"; Role "navigation"; AriaLabel "pagination"] [
                                ul [Class "pagination-list"] [
                                    let resArray = model.FastaFileInputResult.Value
                                    let len = resArray.Length 
                                    yield pageinateFromIndex res 0 model dispatch
                                    if len >5 then yield li [Class "pagination-ellipsis"] [span [] [str "…"]]
                                    yield! pageinateDynamic index res model dispatch
                                    if len >5 then yield li [Class "pagination-ellipsis"] [span [] [str "…"]]
                                    yield pageinateFromIndex res (len-1) model dispatch
                                ]
                            ]
                ]
            ]
    ]

let resultHeading (model:Model) (dispatch: Msg -> unit)  (res: TargetPResult [] ) =
    let index = model.CurrentResultViewIndex
    Columns.columns
        [
            if model.ResultHeadingIsSticky then
                yield Columns.CustomClass "ResultHeadingSticky"
            else
                yield Columns.CustomClass "ResultHeadingNonSticky"
            yield
                Columns.Props
                    [
                        yield Id "ResultHeading"
                        match model.SeqMode with
                        |File   when model.FastaFileInputResult.IsSome  -> ()
                        |Single when model.SingleSequenceResult.IsSome  -> ()
                        | _                                             -> yield Props.Style [Props.Display DisplayOptions.None]
                    ]

        ] [
        Column.column [Column.Width (Screen.Desktop, Column.Is7);Column.CustomClass "leftResultHeading"] [
            Columns.columns [] [
                Column.column [Column.Width (Screen.Desktop, Column.Is3)] []
                Column.column [Column.Width (Screen.Desktop, Column.Is9)] [
                    br []
                    Heading.h3 [] [str "Results"]
                    hr []
                ]
            ]
        ]
        Column.column [Column.Width (Screen.Desktop, Column.Is5);Column.CustomClass "rightResultHeading"] [
            Columns.columns [] [
                Column.column [Column.Width (Screen.Desktop, Column.Is8)] [
                    yield br []
                    if model.SeqMode = File && model.FastaFileInputResult.IsSome then
                        yield
                            nav [Class "pagination is-left"; Role "navigation"; AriaLabel "pagination"] [
                                ul [Class "pagination-list"] [
                                    let resArray = model.FastaFileInputResult.Value
                                    let len = resArray.Length 
                                    yield pageinateFromIndex res 0 model dispatch
                                    if len >5 then yield li [Class "pagination-ellipsis"] [span [] [str "…"]]
                                    yield! pageinateDynamic index res model dispatch
                                    if len >5 then yield li [Class "pagination-ellipsis"] [span [] [str "…"]]
                                    yield pageinateFromIndex res (len-1) model dispatch
                                ]
                            ]
                    else
                        yield Heading.h3 [] [br[]]
                    yield hr []
                    if model.SeqMode = File then
                        yield progressView model.ShowProgressDetails model dispatch
                ]
                Column.column [Column.Width (Screen.Desktop, Column.Is4)] [
                ]
            ]

        ]
    ]

let plotModeSwitch (model : Model) (dispatch: Msg -> unit) =
    Level.level [] [
        Level.left [] [
            Button.button [
                yield Button.IsFullWidth
                if model.PlotMode = Propensity then
                    yield Button.CustomClass "plotModeBtnActive"
                else
                    yield Button.CustomClass "plotModeBtnInactive"
                yield Button.OnClick (fun _ -> ChangePlotMode Propensity |> dispatch)
            ] [
                str "Plot iMTS-L propensity"
            ]
            Button.button [
                yield Button.IsFullWidth
                if model.PlotMode = TargetPScore then
                    yield Button.CustomClass "plotModeBtnActive"
                else
                    yield Button.CustomClass "plotModeBtnInactive"
                yield Button.OnClick (fun _ -> ChangePlotMode TargetPScore |> dispatch)
            ] [
                str "Plot raw TargetP score"
            ]
        ]
        Level.right [] [
        ]
    ]

let singleResult (model : Model) (dispatch: Msg -> unit) (res: TargetPResult) =
    Columns.columns [Columns.IsCentered] [
        Column.column [Column.Width (Screen.Desktop, Column.Is2)] []
        Column.column [Column.CustomClass "transparent fastaDisplay";Column.Width (Screen.Desktop, Column.Is8)] [
            br []
            Heading.h4 [] [str res.Header]
            plotModeSwitch model dispatch
            hr []
            Heading.h4 [] [
                if model.PlotMode = Propensity then
                    yield str "iMTS-L propensity heatmap:"
                else
                    yield str "Raw TargetP sequence score heatmap:"
            ]
            fastaFormatDisplay model (res.Sequence.ToCharArray()) (if model.PlotMode = Propensity then res.Propensity else res.Scores)
            hr []
            Heading.h4 [] [str "Predicted iMTS-L propensity profile:"]
            iframe [
                Props.SrcDoc
                    (
                        if model.PlotMode = Propensity then
                            res.PropensityPlotHtml.Replace("</head>","<style>.js-plotly-plot{width: 50% !important;margin: auto !important;}</style></head>")
                        else
                            res.ScorePlotHtml.Replace("</head>","<style>.js-plotly-plot{width: 50% !important;margin: auto !important;}</style></head>")
                    )
                Props.Class "ResultFrame"
                Props.Scrolling "no"]
                [
                    p [] [
                        str "Your browser does not support the srcDoc attribute of iframes. See https://caniuse.com/#search=iframe for Browser version that support iframes."
                    ]
            ]
            Heading.h4 [] [str "Download your results"]
            hr[]
            downloadView model dispatch
        ]
        Column.column [Column.Width (Screen.Desktop, Column.Is2)] []
    ]

let multipleResults (model : Model) (dispatch: Msg -> unit) (res: TargetPResult array) =
    let index = model.CurrentResultViewIndex
    let sequence = res.[index].Sequence 
    let scores = res.[index].Scores
    let propensity = res.[index].Propensity
    let progr = (float model.FileProcessIndex / float model.FastaFileInput.Length)
    Columns.columns [Columns.IsCentered] [
        Column.column
            [
                Column.Width (Screen.Desktop, Column.Is2)
                Column.Props [OnClick (fun _ -> ChangeViewIndex -1 |> dispatch)]
                Column.CustomClass "viewManipulator"
            ] [
                div [Class "has-text-centered"] [str "<"]
            ]
        Column.column [Column.CustomClass "transparent fastaDisplay";Column.Width (Screen.Desktop, Column.Is8)] [
            br []
            Heading.h4 [] [str res.[index].Header]
            plotModeSwitch model dispatch
            hr []
            Heading.h4 [] [str "Sequence score heatmap:"]
            fastaFormatDisplay model (sequence.ToCharArray()) (if model.PlotMode = Propensity then propensity else scores)
            hr []
            Heading.h4 [] [str "Predicted iMTS-L propensity profile:"]
            iframe [
                Props.SrcDoc
                    (
                        if model.PlotMode = Propensity then
                            res.[index].PropensityPlotHtml.Replace("</head>","<style>.js-plotly-plot{width: 50% !important;margin: auto !important;}</style></head>")
                        else
                            res.[index].ScorePlotHtml.Replace("</head>","<style>.js-plotly-plot{width: 50% !important;margin: auto !important;}</style></head>") 
                    )
                Props.Class "ResultFrame"
                Props.Scrolling "no"]
                [
                    p [] [
                        str "Your browser does not support the srcDoc attribute of iframes. See https://caniuse.com/#search=iframe for Browser version that support iframes."
                    ]
            ]
            Heading.h4 [] [str "Download your results"]
            hr[]
            downloadView model dispatch
        ]
        Column.column
            [
                Column.Width (Screen.Desktop, Column.Is2)
                Column.Props [OnClick (fun _ -> ChangeViewIndex 1 |> dispatch)]
                Column.CustomClass "viewManipulator"
            ] [
                div [Class "has-text-centered"] [str ">"]
            ]
    ]

let resultSection (model : Model) (dispatch: Msg -> unit) =
    Section.section [
                        if model.ShowResults = false then yield Section.Props [Props.Style [Props.Display DisplayOptions.None]] 
                        yield Section.CustomClass "resultSection"
                    ] [
        if model.SeqMode = Single then
            match model.SingleSequenceResult with
            |Some res -> yield singleResult model dispatch res
            |None -> ()
        elif model.SeqMode = File then
            match model.FastaFileInputResult with
            |Some res -> yield multipleResults model dispatch res
            |None -> ()
    ]

let validateInputState (model:Model) =
    match model.SeqMode with
    |Single -> 
        match model.SingleSequence with
        |"" -> false,"No data provided"
        | _ ->  match model.SelectedTargetPModel with
                | NoModel   ->  false,"No model selected"
                | _         ->  match model.HasValidFasta with
                                | true -> true,"Start computation"
                                | false -> false, "Fasta is invalid"
    |File -> 
        match model.FastaFileInput with
        |[||] -> false,"No data provided"
        | _ ->  match model.SelectedTargetPModel with
                | NoModel   ->  false,"No model selected"
                | _         ->  match model.HasValidFasta with
                                | true -> true,"Start computation"
                                | false -> false, "Fasta is invalid"
    |_ -> false,"No data provided"

let targetPModelSelector (isTargetSelector: bool) (model : Model) (dispatch : Msg -> unit) =
    
    let selectionClassPlant = 
        match model.SelectedTargetPModel with
        | Plant when isTargetSelector -> "is-selected-model"
        | _     -> ""

    let selectionClassNonPlant = 
        match model.SelectedTargetPModel with
        | NonPlant when isTargetSelector-> "is-selected-model"
        | _         -> ""

    Columns.columns [] [
        Column.column [Column.CustomClass "has-text-centered"] [
            Heading.h6 [] [str "Plant model"]
            Image.image [
                Image.Option.Is128x128;
                Image.CustomClass (sprintf "is-centered is-inline-block is-rounded %s" selectionClassPlant)] [
                img [
                    Props.Src "/Images/PlantModel.png";
                    Props.OnClick (fun ev -> if not (model.SeqMode = NotSelected) then TargetPModelSelection TargetPModel.Plant |> dispatch)
                ]
            ]
        ]
        Column.column [Column.CustomClass "has-text-centered"] [
            Heading.h6 [] [str "Non-Plant model"]
            Image.image [
                Image.Option.Is128x128;
                Image.CustomClass (sprintf "is-centered is-inline-block is-rounded %s" selectionClassNonPlant)] [
                img [
                    Props.Src "/Images/YeastModel.png"
                    Props.OnClick (fun ev -> if not (model.SeqMode = NotSelected) then TargetPModelSelection TargetPModel.NonPlant |> dispatch)
                ]
            ]
        ]
    ]

let modeSelection (model : Model) (dispatch : Msg -> unit) =
    match model.SeqMode with
    | Single | NotSelected ->
        Textarea.textarea [
            Textarea.Size Size.IsMedium
            
            Textarea.Placeholder "insert a single amino acid sequence in FASTA format (with header)"
            Textarea.OnChange (fun e -> let sequence = !!e.target?value
                                        SingleSequenceInput sequence |> dispatch)

                ] []
    | _ ->
        File.file [File.IsBoxed;File.IsFullWidth;File.HasName] [
            File.label [] [
                singleFileInput [
                    Props.Hidden true
                    OnTextReceived(fun x -> FastaUploadInput (x.Data,x.Name) |> dispatch)
                    ] 
                File.cta [Props [Class "file-cta fastaFileUploadBtn"]] [
                    Heading.h4 [] [str "Click to choose a file"]
                    Icon.icon [] [Fa.i [Fa.Solid.Upload] []]
                ]
                File.name [] [str model.FastaFileInputName]
            ]
        ]

let inputSelection (model : Model) (dispatch : Msg -> unit) =
    let isValidState,buttonMsg = validateInputState model

    let leftHeader,leftAlternative =
        match model.SeqMode with 
        | Single | NotSelected -> "Or upload a ", "file"
        | _ -> "Or insert a single amino acid ", "sequence"

    
    div [] [
        Columns.columns [Columns.CustomClass "ProcessDecision"] [
            Column.column [Column.Width (Screen.Desktop, Column.Is7);Column.CustomClass "leftSelector"] [
                Columns.columns [] [
                    Column.column [Column.Width (Screen.Desktop, Column.Is3)] []
                    Column.column [Column.Width (Screen.Desktop, Column.Is9)] [
                        yield br []
                        yield Heading.h3 [] [str "Input"]
                        yield hr []
                        if (not model.HasValidFasta) then
                            yield p [Class "is-danger"] [str "Your fasta contained invalid characters:"]
                            yield p [Class "is-danger"] [str (sprintf "%A" model.InvalidFastaChars)   ]
                            yield Button.button [Button.CustomClass "is-danger";Button.OnClick (fun _ -> Reset |> dispatch)] [str "Click to reset Input"]
                        yield modeSelection model dispatch
                        yield br []
                        yield
                            Heading.h5 [Heading.IsSubtitle]
                                [
                                    str leftHeader
                                    a [ Class "leftAlternative"
                                        Props.OnClick
                                            (fun _ ->
                                                match model.SeqMode with 
                                                | Single | NotSelected -> SeqModeSelection File |> dispatch
                                                | _ -> SeqModeSelection Single |> dispatch
                                            )] [
                                        str leftAlternative
                                    ]
                                ]
                        yield br []
                    ]
                ]
            ]
            Column.column [Column.Width (Screen.Desktop, Column.Is5);Column.CustomClass "rightSelector"] [
                Columns.columns [] [
                    Column.column [Column.Width (Screen.Desktop, Column.Is8)] [
                        br []
                        Heading.h3 [] [str "Start Prediction"]
                        hr []
                        
                        Button.button [
                            (if isValidState then
                                Button.Disabled false 
                            else 
                                Button.Disabled true)

                            (if isValidState then
                                Button.CustomClass "is-success"
                            else 
                                Button.CustomClass "is-danger" )

                            Button.IsLoading model.HasJobRunning 
                            Button.IsFullWidth
                            Button.CustomClass "startBtn"
                            Button.OnClick (fun e ->
                                match model.SeqMode with
                                | Single    -> SingleSequenceRequest |> dispatch
                                | File      -> FastaUploadRequest |> dispatch
                                | _ -> ())
                        ] [str buttonMsg ]
                        br []
                        Heading.h5 [Heading.IsSubtitle] [str "Select the model that is closest to your organism of interest and start the IMTS prediction:"]
                        Control.div [] [
                            Radio.radio [] [
                                Radio.input
                                    [
                                        if model.SelectedTargetPModel = NonPlant then
                                            yield Radio.Input.Props [Checked true]
                                        yield Radio.Input.Name "ModelSelection"
                                        yield Radio.Input.Props [OnClick (fun _ -> TargetPModelSelection TargetPModel.NonPlant |> dispatch)]
                                    ]
                                b [] [str "Non-Plant (default)"]
                            ]
                            Radio.radio [] [
                                Radio.input
                                    [
                                        Radio.Input.Name "ModelSelection"
                                        Radio.Input.Props [OnClick (fun _ -> TargetPModelSelection TargetPModel.Plant |> dispatch)]
                                    ]
                                b [] [str "Plant (experimental)"]
                            ]
                        ]
                        br []
                    ]
                    Column.column [Column.Width (Screen.Desktop, Column.Is4)] []
                ]
                
            ]
        ]
    ]

let hero (model : Model) (dispatch : Msg -> unit) =
    Hero.hero [Hero.IsMedium; Hero.CustomClass "csbHero"] [
        Hero.body [] [
            Container.container [] [
                br []
                br []
                Heading.h1 [] [
                    str "iMTS-Ls Prediction"
                ]
                br []
                Heading.h3 [Heading.IsSubtitle] [
                    str "Additional to their N-terminal matrix-targeting signals (MTSs), many preproteins contain additional internal MTS-like signals (iMTS-Ls) in their mature region that share similar characteristic properties. Tom70-mediated interaction with these iMTS-Ls improves the import competence of preproteins and increases the efficiency of their translocation into the mitochondrial matrix."
                    br []
                    br []
                    str"For more information see Backes et al. 2018 [DOI: 10.1083/jcb.201708044]."
                    br []
                    br []
                    str "This tool allows the prediction of iMTS-Ls for proteins of interest."
                    ]
            ]
        ]
    ]

let errorDisplay (model : Model) (dispatch : Msg -> unit) =
    let msg,stackTrace =
        match model.ErrorState with
        | Some ex  -> match ex with
                      | :? Fable.Remoting.Client.ProxyRequestException as fe -> fe.Message, fe.ResponseText
                      | _ -> ex.Message,ex.StackTrace
        | None      -> "Unexpected Error","App failed without Exception message"
    Section.section [Section.CustomClass "ErrorSection"] [
        Heading.h1 []
            [
            Icon.icon [Icon.IsRight; Icon.Size IsLarge] [Fa.i [Fa.Solid.SkullCrossbones] []]
            str "Oopsie!"
            Icon.icon [Icon.IsRight; Icon.Size IsLarge] [Fa.i [Fa.Solid.SkullCrossbones] []]
            ]

        Heading.h2 [] [str "An error occured. Click the button below to reset the app state:"]
        Button.button [Button.CustomClass "is-danger resetBtn";Button.OnClick (fun _ -> Reset |> dispatch)] [str "RESET APP STATE"]
        br []
        br []
        Heading.h3 [] [str "If you are a developer and/or interested in the stack trace you can see the error message below."]
        br []
        Content.content [] [
            Heading.h3 [] [str msg]
            Heading.h3 [] [str "StackTrace:"]
            Heading.h5 [] [str stackTrace]
        ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    if model.HasError then
        errorDisplay model dispatch
    else
        div [][
            navbar model dispatch
            displayHelpSection model dispatch
            hero model dispatch
            inputSelection model dispatch
            resultHeading model dispatch (match model.FastaFileInputResult with |Some r -> r | _ -> [||])
            resultSection model dispatch
            Footer.footer [] [
                Container.container [] [
                    Columns.columns [] [
                        Column.column [] [
                        
                            str "built with <3,"
                            ul [] [
                                br []
                                li [Props.Href "" ] [a [] [str "F#"; str","]]
                                br []
                                li [Props.Href "" ] [a [] [str "BioFSharp"]; str","]
                                br []
                                li [Props.Href "" ] [str "and ";a [] [str "SAFE Stack"]]
                            ] 
                        ]
                        Column.column [] [
                            str "This service uses the targetP under the hood."
                            br []
                            br []
                            str "For more information about targetP, head "
                            a [Props.Href "http://www.cbs.dtu.dk/services/TargetP/"] [str "here"]
                        ]
                        Column.column [] [
                            str "Verion 0.1.0."
                            br []
                            br []
                            str " This service is developed and maintained by the "
                            a [Props.Href ""] [str "Computational Systems Biology department "]
                            str "of the TU Kaiserslautern, Germany."
                            br []
                            br []
                            a [Props.Href ""] [str "SourceCode available here"]
                        ]
                    ]
                ]            
            ]
        ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run

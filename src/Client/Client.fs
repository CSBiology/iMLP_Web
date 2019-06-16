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


open FileInputHelper
open FileInputHelper.React
open System

type Mode =
|NotSelected
|Single
|File


// The model holds data that you want to keep track of while the application is running
type Model = { 
    SessionGuid             :   System.Guid
    BurgerVisible           :   bool
    SelectedTargetPModel    :   TargetPModel
    SingleSequence          :   string
    SingleSequenceResult    :   TargetPResult Option
    FastaFileInputText      :   string
    FastaFileInputResult    :   (TargetPResult array) Option
    SeqMode                 :   Mode
    ShowResults             :   bool
    CurrentResultViewIndex  :   int
    DownloadReady           :   bool
    DownloadFileName        :   string
}

let initialModel = {
    SessionGuid             =   System.Guid.NewGuid()
    BurgerVisible           =   false
    SelectedTargetPModel    =   TargetPModel.NoModel
    SingleSequence          =   ""
    SingleSequenceResult    =   None
    FastaFileInputText      =   ""
    FastaFileInputResult    =   None
    SeqMode                 =   Mode.NotSelected
    ShowResults             =   false
    CurrentResultViewIndex  =   0
    DownloadReady           =   false
    DownloadFileName        =   "IMTS_prediction_results.tsv" 
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
| ToggleBurger
| TargetPModelSelection     of TargetPModel
| FastaUploadInput          of string
| SingleSequenceInput       of string
| SingleSequenceRequest
| SingleSequenceResponse    of Result<TargetPResult,exn>
| FastaUploadRequest
| FastaUploadResponse       of Result<(TargetPResult array),exn>
| ShowPlot                  of int 
| PrepareDownloadCSV
| DownloadResponse          of Result<unit,exn>
| DownloadFileNameChange    of string

let gradientColorTable = [| 
    "#D62728"
    "#D42828"
    "#D32928"
    "#D22B28"
    "#D12C29"
    "#D02D29"
    "#CF2F29"
    "#CE302A"
    "#CD312A"
    "#CC332A"
    "#CB342B"
    "#CA352B"
    "#C9372B"
    "#C8382C"
    "#C7392C"
    "#C63B2C"
    "#C53C2C"
    "#C43D2D"
    "#C33F2D"
    "#C2402D"
    "#C1412E"
    "#C0432E"
    "#BF442E"
    "#BE452F"
    "#BD472F"
    "#BC482F"
    "#BB4930"
    "#BA4B30"
    "#B94C30"
    "#B84D30"
    "#B74F31"
    "#B65031"
    "#B55131"
    "#B45332"
    "#B35432"
    "#B25532"
    "#B15733"
    "#B05833"
    "#AF5933"
    "#AE5B34"
    "#AD5C34"
    "#AC5D34"
    "#AB5F35"
    "#AA6035"
    "#A96135"
    "#A86335"
    "#A76436"
    "#A66536"
    "#A56736"
    "#A46837"
    "#A36A37"
    "#A16B37"
    "#A06C38"
    "#9F6E38"
    "#9E6F38"
    "#9D7039"
    "#9C7239"
    "#9B7339"
    "#9A7439"
    "#99763A"
    "#98773A"
    "#97783A"
    "#967A3B"
    "#957B3B"
    "#947C3B"
    "#937E3C"
    "#927F3C"
    "#91803C"
    "#90823D"
    "#8F833D"
    "#8E843D"
    "#8D863E"
    "#8C873E"
    "#8B883E"
    "#8A8A3E"
    "#898B3F"
    "#888C3F"
    "#878E3F"
    "#868F40"
    "#859040"
    "#849240"
    "#839341"
    "#829441"
    "#819641"
    "#809742"
    "#7F9842"
    "#7E9A42"
    "#7D9B42"
    "#7C9C43"
    "#7B9E43"
    "#7A9F43"
    "#79A044"
    "#78A244"
    "#77A344"
    "#76A445"
    "#75A645"
    "#74A745"
    "#73A846"
    "#72AA46"
    "#71AB46"
    "#70AD47"

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
        updatedModel,Cmd.none
    | FastaUploadInput file -> 
        let seqMode =
            if file.Length > 0 then 
                Mode.File
            else
                Mode.NotSelected
        let updatedModel = {currentModel with FastaFileInputText = file; SeqMode = seqMode}
        updatedModel,Cmd.none
    | SingleSequenceRequest ->
        let updatedModel = {currentModel with DownloadReady = false}
        let requestCmd = 
            Cmd.ofAsync
                (Server.targetPApi.SingleSequenceRequest currentModel.SelectedTargetPModel)
                currentModel.SingleSequence
                (Ok >> SingleSequenceResponse)
                (Error >> SingleSequenceResponse)
        updatedModel,requestCmd
    | FastaUploadRequest ->
        let updatedModel = {currentModel with DownloadReady = false}
        let requestCmd = 
            Cmd.ofAsync
                (Server.targetPApi.FastaFileRequest currentModel.SelectedTargetPModel)
                currentModel.FastaFileInputText
                (Ok >> FastaUploadResponse)
                (Error >> FastaUploadResponse)
        updatedModel,requestCmd

    | SingleSequenceResponse (Ok res) ->
        let updatedModel = {
            currentModel with 
                SingleSequenceResult = Some res
                ShowResults = true
                }
        updatedModel,Cmd.none

    | SingleSequenceResponse (Error res) ->
        console.log("ERROROROROROROROROROOROROOROROR")
        console.log(res)
        currentModel,Cmd.none

    | FastaUploadResponse (Ok res) ->
        let updatedModel = {
            currentModel with 
                FastaFileInputResult = Some res
                ShowResults = true
                }
        updatedModel,Cmd.none

    | FastaUploadResponse (Error res) ->
        console.log(res)
        currentModel,Cmd.none

    | ShowPlot index ->
        let updatedModel = {currentModel with CurrentResultViewIndex = index}
        updatedModel,Cmd.none

    | PrepareDownloadCSV ->
        match currentModel.SeqMode with
        |Single ->
            match currentModel.SingleSequenceResult with
            |Some res ->
                let requestCmd = 
                    Cmd.ofAsync
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
                        Cmd.ofAsync
                            Server.targetPApi.DownloadRequestMultiple
                            (res,currentModel.SessionGuid)
                            (Ok >> DownloadResponse)
                            (Error >> DownloadResponse)
                    currentModel,requestCmd
                |_ -> currentModel,Cmd.none
        |_ -> currentModel,Cmd.none
    | DownloadResponse (Ok _) ->
        let updatedModel = {currentModel with DownloadReady = true}
        updatedModel,Cmd.none
    | DownloadResponse (Error ex) ->
        let updatedModel = {currentModel with DownloadReady = false}
        updatedModel,Cmd.none
    | DownloadFileNameChange dname ->
        let updatedModel = {currentModel with DownloadFileName = dname}
        updatedModel,Cmd.none

let navbar (model : Model) (dispatch : Msg -> unit) =
    Navbar.navbar [Navbar.IsFixedTop; Navbar.CustomClass "is-dark csbNav"; Navbar.Props [Props.Role "navigation"; AriaLabel "main navigation" ]] [
        Navbar.Brand.a [] [
            Navbar.Item.a [] [
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
                Navbar.Item.a [] [
                    str "Lorem"
                ]
                Navbar.Item.a [] [
                    str "Ipsum "
                ]
                Navbar.Item.a [] [
                    str "Dolor"
                ]
            ]
            Navbar.End.div [] [
                Navbar.Item.a [] [
                    str "Lorem"
                ]
                Navbar.Item.a [] [
                    str "Ipsum"
                ]
                Navbar.Item.a [] [
                    str "Dolor"
                ]
            ]
        ]
    ]

let hero = 
    Hero.hero [Hero.IsMedium; Hero.CustomClass "csbHero"] [
        Hero.body [] [
            Container.container [] [
                Heading.h1 [] [
                    str "Lorem ipsum dolor sit amet"
                ]
                Heading.h3 [Heading.IsSubtitle] [
                    str "consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat"
                ]
            ]
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

let fastaFormatDisplay (sequence:char array) (scores: float array) =
    let maxVal = Array.max scores
    let norm = 
        scores
        |> Array.map (fun s -> s/maxVal)
        |> Array.map2 (fun char score -> int ((score) * 100.) ,char) sequence
    let spans =
        norm 
        |> Array.map (fun (score,char) -> span [Props.Style [
                                                                    CSSProp.BackgroundColor (gradientColorTable.[score]);
                                                                    CSSProp.Color "white"
                                                                    
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
    Content.content [Content.CustomClass "is-small"] [
        yield! formatStrings 
    ]

let downloadBtn (location:string) (model:Model) (dispatch: Msg -> unit)=
    [
        div [Props.Class "card-footer-item"] [
            str "Enter filename:"
        ]
        Input.text [
                    Input.Props [Props.Class "card-footer-item"]
                    Input.Placeholder model.DownloadFileName
                    Input.OnChange (fun e ->    let dname = !!e.target?value
                                                DownloadFileNameChange dname |> dispatch)
                    ] 
        a[
            Props.Download (  if ( model.DownloadFileName.EndsWith(".tsv")) then
                                    model.DownloadFileName
                                else 
                                    sprintf "%s.tsv" model.DownloadFileName
                                    )
            Props.Href location
            Props.Class "card-footer-item is-primary"
            ] [
            Icon.icon [] [Fa.i [Fa.Solid.Download] []]
            str "Click to download"
        ]
    ]

let downloadView (model:Model) (dispatch: Msg -> unit) =
    if model.DownloadReady then
        downloadBtn (sprintf "./CsvResults/%s.csv" (model.SessionGuid.ToString())) model dispatch
    else
        [
            Button.button [
                        Button.Props [Props.Id "prepareDownload"]
                        Button.CustomClass "card-footer-item is-success" 
                        Button.IsFullWidth
                        Button.OnClick (fun _ -> PrepareDownloadCSV |> dispatch)] [
                str "Prepare Results as tab separated file for download"
            ]
        ]

let singleResult (model : Model) (dispatch: Msg -> unit) (res: TargetPResult) =
    Card.card [] [
        Card.header [] [Heading.h3 [] [str "IMTS prediction results "]]
        Card.content [] [
            Heading.h4 [] [str "Header"]
            Heading.h6 [] [str res.Header]
            br []
            hr []
            br []
            Columns.columns [] [
                Column.column [Column.CustomClass "transparent fastaDisplay";Column.Width (Screen.Desktop, Column.Is6)] [
                    br []
                    br []
                    br []
                    Heading.h4 [] [str "Full Sequence"]
                    fastaFormatDisplay (res.Sequence.ToCharArray()) res.Scores
                    
                ]
                Column.column [Column.Width (Screen.Desktop, Column.Is6); Column.Props [Props.Style [CSSProp.OverflowY "hidden"]]] [
                    iframe [    Props.SrcDoc res.PlotHtml 
                                Props.Class "ResultFrame"
                                Props.Scrolling "no"
                                    ] [p[] [str "Your browser does not support the srcDoc attribute of iframes. See https://caniuse.com/#search=iframe for Browser version that support iframes."]
                                    
                    ]
                ]
            ]
        ]
        Card.footer [] [
            yield! downloadView model dispatch
        ]
    ]
let multipleResults (model : Model) (dispatch: Msg -> unit) (res: TargetPResult array) =
    let index = model.CurrentResultViewIndex
    let sequence = res.[index].Sequence 
    let scores = res.[index].Scores 
    Card.card [] [
        Card.header [] [Heading.h3 [] [str "IMTS prediction results "]]
        Card.content [] [
            Heading.h4 [] [str "Header"]
            Heading.h6 [] [str res.[index].Header]
            br []
            hr []
            br []
            nav [Class "pagination is-centered"; Role "navigation"; AriaLabel "pagination"] [
                ul [Class "pagination-list"] [
                    let resArray = model.FastaFileInputResult.Value
                    let len = resArray.Length 
                    yield pageinateFromIndex res 0 model dispatch
                    if len>5 then yield li [Class "pagination-ellipsis"] [span [] [str "…"]]
                    yield! pageinateDynamic index res model dispatch
                    if len>5 then yield li [Class "pagination-ellipsis"] [span [] [str "…"]]
                    yield pageinateFromIndex res (len-1) model dispatch
                ]
            ]
            Columns.columns [] [
                Column.column [Column.CustomClass "transparent fastaDisplay";Column.Width (Screen.Desktop, Column.Is6)] [
                    br []
                    br []
                    br []
                    Heading.h4 [] [str "Full Sequence"]
                    fastaFormatDisplay (sequence.ToCharArray()) scores
                    
                ]
                Column.column [Column.Width (Screen.Desktop, Column.Is6); Column.Props [Props.Style [CSSProp.OverflowY "hidden"]]] [
                    iframe [    Props.SrcDoc res.[index].PlotHtml 
                                Props.Class "ResultFrame"
                                Props.Scrolling "no"
                                    ] [p[] [str "Your browser does not support the srcDoc attribute of iframes. See https://caniuse.com/#search=iframe for Browser version that support iframes."]]
                ]
            ]
        ]
        Card.footer [] [
            yield! downloadView model dispatch
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

//let results (model : Model) (dispatch : Msg -> unit) =
//    match model.SeqMode with
//    |Single ->
//        Section.section [Section.CustomClass "csbSection"] [
//            Container.container [] [
//                resultCard model model.SingleSequenceResult.Value
//            ]
//        ]
//    |File ->
//        R. div [] []
//    |_ -> 
//        R. div [] []

let validateInputState (model:Model) =
    match model.SeqMode with
    |Single -> 
        match model.SingleSequence with
        |"" -> false,"No data provided"
        | _ ->  match model.SelectedTargetPModel with
                | NoModel   -> false,"No model selected"
                | _         -> true, "Start computation"
    |File -> 
        match model.FastaFileInputText with
        |"" -> false,"No data provided"
        | _ ->  match model.SelectedTargetPModel with
                | NoModel   -> false,"No model selected"
                | _         -> true, "Start computation"
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


let view (model : Model) (dispatch : Msg -> unit) =
    let isValidState,buttonMsg = validateInputState model
    let isSingle = model.SeqMode = Single
    let isFile = model.SeqMode = File
    div [] [
        navbar model dispatch
        hero
        Section.section [Section.CustomClass "csbSection"] [
            Container.container [] [
                Heading.h3 [] [str "Lorem ipsum dolor sit amet"]
                Heading.h4 [] [str "consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat"]            
                Tile.ancestor [] [
                    Tile.child [Tile.Size Tile.Is5; Tile.CustomClass "notification csbTile"] [
                        Heading.h4 [] [str "LOREM"]
                        p [] [str "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin ornare magna eros, eu pellentesque tortor vestibulum ut. Maecenas non massa sem. Etiam finibus odio quis feugiat facilisis."]
                        Textarea.textarea [
                                            Textarea.Placeholder "insert a single amino acid sequence here"
                                            Textarea.OnChange (fun e -> let sequence = !!e.target?value
                                                                        SingleSequenceInput sequence |> dispatch)
                                        
                        ] []
                        br []
                        targetPModelSelector isSingle model dispatch
                        Button.button [
                            (if isValidState && (model.SeqMode = Single) then
                                Button.Disabled false 
                            else 
                                Button.Disabled true)

                            (if isValidState && (model.SeqMode = Single) then
                                Button.CustomClass "is-success"
                            else 
                                Button.CustomClass "is-danger" )
                            Button.OnClick (fun e -> SingleSequenceRequest |> dispatch)
                        ] [str (if not isFile then buttonMsg else "Insert or Update Sequence")]
                    ]
                    Tile.child [Tile.Size Tile.Is2][]
                    Tile.child [Tile.Size Tile.Is5; Tile.CustomClass "notification csbTile"] [
                        Heading.h4 [] [str "LOREM"]
                        p [] [str "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin ornare magna eros, eu pellentesque tortor vestibulum ut. Maecenas non massa sem. Etiam finibus odio quis feugiat facilisis."]
                        File.file [File.IsBoxed;File.IsCentered] [
                            File.label [] [
                                singleFileInput [
                                    Props.Hidden true
                                    OnTextReceived(fun x -> FastaUploadInput x.Data |> dispatch)
                                    ] 
                                File.cta [] [
                                    Icon.icon [] [Fa.i [Fa.Solid.Download] []]
                                ]
                            ]
                        ]
                        br []
                        targetPModelSelector (not isSingle) model dispatch
                        Button.button [
                            (if isValidState && (model.SeqMode = File) then
                                Button.Disabled false 
                            else 
                                Button.Disabled true)

                            (if isValidState && (model.SeqMode = File) then
                                Button.CustomClass "is-success"
                            else 
                                Button.CustomClass "is-danger" )
                            Button.OnClick (fun e -> FastaUploadRequest |> dispatch)
                        ] [str (if not isSingle then buttonMsg else "Upload or Update Fasta File")]
                    ] 
                ]
            ]
        ]
        resultSection model dispatch
        Footer.footer [] [
            Container.container [] [
                Content.content [Content.CustomClass "has-text-centered"] [
                    p [] [
                        str "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin ornare magna eros, eu pellentesque tortor vestibulum ut. Maecenas non massa sem. Etiam finibus odio quis feugiat facilisis."
                    ]
                ]
                Columns.columns [] [
                    Column.column [] [
                        str "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin ornare magna eros, eu pellentesque tortor vestibulum ut. Maecenas non massa sem. Etiam finibus odio quis feugiat facilisis."
                    ]
                    Column.column [] [
                        str "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin ornare magna eros, eu pellentesque tortor vestibulum ut. Maecenas non massa sem. Etiam finibus odio quis feugiat facilisis."
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

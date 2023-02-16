module ResultViews

open Elmish
open Elmish.React

open Fable.React.Props
open Fable.Core.JsInterop
open Shared
open Fulma
//open Fulma.FontAwesome
open Fable.React
open Browser.Dom
open Browser.Types

open Shared
open JSInterop
open AppModel
open StateHandling

open GenericViewComponents

module ViewComponents = 

    let pageinateFromIndex (index:int) (model:Model) (dispatch:Msg->unit)  =
        let ofClass () = 
            if index = model.CurrentResultViewIndex then 
                Props.Class"pagination-link is-current"
            else 
                Props.Class "pagination-link"
        li [
            ofClass ()
            Props.OnClick (fun _ -> ShowPlot (index) |> dispatch)
        ] [
            span [] [str (string (index+1))]
        ]

    let pageinateDynamic (pos: int) (model:Model) (dispatch:Msg->unit) = 
        let resultLenght = Model.getMultiResultLength model
        let indices =
            [0 .. resultLenght-1]
        let numbers =
            let x = indices.[(max 1 (pos-2)) .. (min (pos+2) (resultLenght-2)) ]
            x
            |> List.map (fun index -> pageinateFromIndex index model dispatch) 
        numbers


    let progressView (model : Model)  (dispatch: Msg -> unit)  =
        let progr = (float model.FileProcessIndex / float model.FastaFileInput.Length)
        div [] [
            if model.HasJobRunning then 
                yield str (if progr < 1. then sprintf "Progress: %i/%i Proteins" model.FileProcessIndex model.FastaFileInput.Length else "Done.")
                yield progress [Class "progress is-link is-large"; Props.Value progr] [str (sprintf "%.2f%s" (float model.FileProcessIndex / float model.FastaFileInput.Length) "%")]
        ]

    let resultBar (model : Model) (dispatch : Msg -> unit) =
        let index = model.CurrentResultViewIndex
        let hasMultiResult = Model.hasMultiResult model
        div
            [
                yield Props.Class "resultBar"
                if (not (model.SeqMode = File && hasMultiResult)) then
                    yield Props.Style [Props.Display DisplayOptions.None]
            ] [
                Columns.columns [] [
                    Column.column [Column.Width (Screen.Desktop, Column.Is7);Column.CustomClass "leftResultBar"] [
                        Heading.h3 [] [str "Navigate through results:"]
                    ]
                    Column.column [Column.Width (Screen.Desktop, Column.Is7);Column.CustomClass "rightResultBar"] [
                        if model.SeqMode = File && hasMultiResult then
                            yield
                                nav [Class "pagination is-left"; Role "navigation"; AriaLabel "pagination"] [
                                    ul [Class "pagination-list"] [
                                        let len = Model.getMultiResultLength model
                                        yield pageinateFromIndex 0 model dispatch
                                        if len >5 then yield li [Class "pagination-ellipsis"] [span [] [str "…"]]
                                        yield! pageinateDynamic index model dispatch
                                        if len >5 then yield li [Class "pagination-ellipsis"] [span [] [str "…"]]
                                        yield pageinateFromIndex (len-1) model dispatch
                                    ]
                                ]
                    ]
                ]
        ]

    let resultHeading (model:Model) (dispatch: Msg -> unit) =
        let index = model.CurrentResultViewIndex
        let hasMultiResult = Model.hasMultiResult model
        let hasSingleResult = Model.hasMultiResult model
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
                            |File   when hasMultiResult -> ()
                            |Single when hasSingleResult-> ()
                            | _ -> yield Props.Style [Props.Display DisplayOptions.None]
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
                        if model.SeqMode = File && hasMultiResult then
                            yield
                                nav [Class "pagination is-left"; Role "navigation"; AriaLabel "pagination"] [
                                    ul [Class "pagination-list"] [
                                        let len = Model.getMultiResultLength model
                                        yield pageinateFromIndex 0 model dispatch
                                        if len >5 then yield li [Class "pagination-ellipsis"] [span [] [str "…"]]
                                        yield! pageinateDynamic index model dispatch
                                        if len >5 then yield li [Class "pagination-ellipsis"] [span [] [str "…"]]
                                        yield pageinateFromIndex (len-1) model dispatch
                                    ]
                                ]
                        else
                            yield Heading.h3 [] [br[]]
                        yield hr []
                        if model.SeqMode = File then
                            yield progressView model dispatch
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

    
    let fastaFormatDisplay (model:Model) (sequence:char array) (scores: float array) =
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
                //console.log(sprintf "score length: %i" x.Length)
                //console.log(sprintf "sequence length: %i" sequence.Length)
                //console.log(sequence)
                x |> Array.map2 (fun char score -> int ((score) * 100.) ,char) sequence
        let spans =
            norm 
            |> Array.mapi
                (fun i (score,char) ->
                    span [
                        Props.Style [
                            yield CSSProp.BackgroundColor (gradientColorTable.[int (round (float score / 10.))])
                            yield CSSProp.Color "#44546A"
                            ]] [b [] [str (string char)
                        ]
                    ]
                )
        let formatStrings = 
            spans
            |> Array.chunkBySize 60
            |> Array.mapi
                (fun i spans ->
                    let lineIndex = (i * 60 + 1)
                    let indexText =
                        sprintf
                            "%i%s"
                            lineIndex
                            ([for x in 1 .. (10 - (string lineIndex).Length) do yield "."] |> String.concat "")
                    div [] [  
                        yield span [] [ str indexText]; 
                        yield! spans 
                    ]
                )
        let minLegend = Array.min scores
        let maxLegend = Array.max scores
        Content.content [] [
            yield br []
            yield! formatStrings 
            yield br []
            yield
                div[Class "gradient-legend"] [str ""]
                
            yield
                div [] [
                    span [Style [TextAlign TextAlignOptions.Left]] [ str (sprintf "%.2f" minLegend) ]
                    div [Class "spacer"] []
                    span [Style [TextAlign TextAlignOptions.Right]] [ str (sprintf "%.2f" maxLegend) ]
                ]
        ]

module CompositeViews =

    let parseStateMessage (p:ParseState) =
        
        let importance = 
            match p with
            | Success _ ->
                Notification.Color Color.IsSuccess
            | ContainsGapTerOJ _ | FilteredShortSequence _ | ShortSequence _ ->
                Notification.Color Color.IsWarning
            | InvalidCharacters _ | FilteredEmptySequence | EmptySequence | InternalServerError ->
                Notification.Color Color.IsDanger

        Notification.notification [importance] [
            match p with
            | Success _ -> str "Success"
            | ContainsGapTerOJ sanitized ->
                block [h3 [Class "title"] [str "Warning" ]]
                block [str "Input sequence contained Gap,Terminator,Pyrrolysine, and/or J(Xle -Leucine or Isoleucine) characters ('*'/'-'/O/J) and was filtered for them"]
                block [str "Sanitized input:"]
                block [str sanitized]
            | ShortSequence sanitized ->
                block [h3 [Class "title"] [str "Warning" ]]
                block [str "Input sequence was short (<=21 amino acids). Sequences of this length may likely not contain iMTS-Ls."]
            | FilteredShortSequence sanitized ->
                block [h3 [Class "title"] [str "Warning" ]]
                block [str "Input sequence contained Gap,Terminator,Pyrrolysine, and/or J(Xle -Leucine or Isoleucine) characters ('*'/'-'/O/J) and was filtered for them"]
                block [str "Sanitized input:"]
                block [str sanitized]
                block [str "Additionally this input sequence was short (<=21 amino acids). Sequences of this length may likely not contain iMTS-Ls."]
            | InvalidCharacters inv ->
                block [h3 [Class "title"] [str "Error" ]]
                block [str "Input sequence contained invalid characters"]
                block [str "Invalid Characters:"]
                block [str (inv |> Array.distinct |> Array.fold (fun acc elem -> sprintf "%s, %c" acc elem) "")]
            | FilteredEmptySequence ->
                block [h3 [Class "title"] [str "Error" ]]
                block [str "Input sequence was empty after filtering illegal characters and/or Gap,Terminator,Pyrrolysine, and/or J(Xle -Leucine or Isoleucine)  ('*'/'-'/O/J)"]
            | EmptySequence ->
                block [h3 [Class "title"] [str "Error" ]]
                block [str "Input sequence was empty"]
            | InternalServerError ->
                block [h3 [Class "title"] [str "Error" ]]
                block [str "Internal server error during prediction of this protein. Please contact us if this error persists."]

        ]

    module Legacy = 

        let singleResult (model : Model) (dispatch: Msg -> unit) (res: LegacyResult) =
            Columns.columns [Columns.IsCentered] [
                Column.column [Column.Width (Screen.Desktop, Column.Is2)] []
                Column.column [Column.CustomClass "transparent fastaDisplay";Column.Width (Screen.Desktop, Column.Is8)] [
                    match res.ParseState with
                    | Success sanitized | ContainsGapTerOJ sanitized | FilteredShortSequence sanitized | ShortSequence sanitized ->
                        br []
                        Heading.h4 [] [str res.Header]
                        ViewComponents.plotModeSwitch model dispatch
                        hr []
                        parseStateMessage res.ParseState
                        Heading.h4 [] [
                            if model.PlotMode = Propensity || model.SelectedComputationMode = ComputationMode.IMLP then
                                yield str "iMTS-L propensity heatmap:"
                            else
                                yield str "Raw TargetP sequence score heatmap:"
                        ]
                        ViewComponents.fastaFormatDisplay
                            model
                            (sanitized.ToCharArray())
                            (
                                if model.PlotMode = Propensity then
                                    res.PropensityScores
                                else
                                    res.RawTargetPScores
                            )
                        hr []
                        Heading.h4 [] [str (if model.PlotMode = PlotMode.Propensity || model.SelectedComputationMode = ComputationMode.IMLP then "Predicted iMTS-L propensity profile:" else "Predicted raw TargetP scores:")]
                        iframe [
                            Props.SrcDoc
                                (
                                    if model.PlotMode = Propensity || model.SelectedComputationMode = ComputationMode.IMLP then
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
                    | InvalidCharacters _ | FilteredEmptySequence | EmptySequence | InternalServerError ->
                        br []
                        Heading.h4 [] [str res.Header]
                        ViewComponents.plotModeSwitch model dispatch
                        hr []
                        parseStateMessage res.ParseState
                ]
                Column.column [Column.Width (Screen.Desktop, Column.Is2)] []
            ]

        let multipleResults (model : Model) (dispatch: Msg -> unit) (res: LegacyResult array) =
            let index = model.CurrentResultViewIndex
            let sequence = res.[index].Sequence 
            let scores = res.[index].RawTargetPScores
            let propensity = res.[index].PropensityScores
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

                    let currentRes = res.[index]

                    match currentRes.ParseState with
                    | Success _ | ContainsGapTerOJ _ | FilteredShortSequence _ | ShortSequence _ ->
                        parseStateMessage currentRes.ParseState
                        br []
                        Heading.h4 [] [str res.[index].Header]
                        ViewComponents.plotModeSwitch model dispatch
                        hr []
                        Heading.h4 [] [str "Sequence score heatmap:"]
                        ViewComponents.fastaFormatDisplay
                            model
                            (currentRes.Sequence.ToCharArray())
                            (if model.PlotMode = Propensity then currentRes.PropensityScores else currentRes.RawTargetPScores)
                        hr []
                        Heading.h4 [] [str "Predicted iMTS-L propensity profile:"]
                        iframe [
                            Props.SrcDoc
                                (
                                    if model.PlotMode = Propensity then
                                        currentRes.PropensityPlotHtml.Replace("</head>","<style>.js-plotly-plot{width: 50% !important;margin: auto !important;}</style></head>")
                                    else
                                        currentRes.ScorePlotHtml.Replace("</head>","<style>.js-plotly-plot{width: 50% !important;margin: auto !important;}</style></head>") 
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
                    | InvalidCharacters _ | FilteredEmptySequence | EmptySequence | InternalServerError ->
                        br []
                        Heading.h4 [] [str currentRes.Header]
                        ViewComponents.plotModeSwitch model dispatch
                        hr []
                        parseStateMessage currentRes.ParseState
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

    module IMLP = 

        let singleResult (model : Model) (dispatch: Msg -> unit) (res: IMLPResult) =
            Columns.columns [Columns.IsCentered] [
                Column.column [Column.Width (Screen.Desktop, Column.Is2)] []
                Column.column [Column.CustomClass "transparent fastaDisplay";Column.Width (Screen.Desktop, Column.Is8)] [
                    match res.ParseState with
                    | Success sanitized | ContainsGapTerOJ sanitized | FilteredShortSequence sanitized | ShortSequence sanitized ->
                        br []
                        Heading.h4 [] [str res.Header]
                        hr []
                        Heading.h4 [] [
                            if model.PlotMode = Propensity || model.SelectedComputationMode = ComputationMode.IMLP then
                                yield str "iMTS-L propensity heatmap:"
                            else
                                yield str "Raw TargetP sequence score heatmap:"
                        ]
                        ViewComponents.fastaFormatDisplay
                            model
                            (sanitized.ToCharArray())
                            res.PropensityScores
                        hr []
                        Heading.h4 [] [str (if model.PlotMode = PlotMode.Propensity || model.SelectedComputationMode = ComputationMode.IMLP then "Predicted iMTS-L propensity profile:" else "Predicted raw TargetP scores:")]
                        iframe [
                            Props.SrcDoc (res.PropensityPlotHtml.Replace("</head>","<style>.js-plotly-plot{width: 50% !important;margin: auto !important;}</style></head>"))
                            Props.Class "ResultFrame"
                            Props.Scrolling "no"]
                            [
                                p [] [
                                    str "Your browser does not support the srcDoc attribute of iframes. See https://caniuse.com/#search=iframe for Browser version that support iframes."
                                ]
                        ]
                        hr[]
                        Heading.h4 [] [str "RunInfo"]
                        parseStateMessage res.ParseState
                        Heading.h4 [] [str "Download your results"]
                        hr[]
                        downloadView model dispatch
                    | InvalidCharacters _ | FilteredEmptySequence | EmptySequence | InternalServerError ->
                        br []
                        Heading.h4 [] [str res.Header]
                        hr []
                        Heading.h4 [] [str "RunInfo"]
                        parseStateMessage res.ParseState
                ]
                Column.column [Column.Width (Screen.Desktop, Column.Is2)] []
            ]

        let multipleResults (model : Model) (dispatch: Msg -> unit) (res: IMLPResult array) =
            let index = model.CurrentResultViewIndex
            let sequence = res.[index].Sequence 
            let propensity = res.[index].PropensityScores
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
                    let currentRes = res.[index]
                    
                    match currentRes.ParseState with
                    | Success _ | ContainsGapTerOJ _ | FilteredShortSequence _ | ShortSequence _ ->
                        br []
                        Heading.h4 [] [str res.[index].Header]
                        hr []
                        Heading.h4 [] [str "Sequence score heatmap:"]
                        ViewComponents.fastaFormatDisplay
                            model
                            (res.[index].Sequence.ToCharArray())
                            res.[index].PropensityScores
                        hr []
                        Heading.h4 [] [str "Predicted iMTS-L propensity profile:"]
                        iframe [
                            Props.SrcDoc (res.[index].PropensityPlotHtml.Replace("</head>","<style>.js-plotly-plot{width: 50% !important;margin: auto !important;}</style></head>") )
                            Props.Class "ResultFrame"
                            Props.Scrolling "no"]
                            [
                                p [] [
                                    str "Your browser does not support the srcDoc attribute of iframes. See https://caniuse.com/#search=iframe for Browser version that support iframes."
                                ]
                        ]
                        hr[]
                        Heading.h4 [] [str "RunInfo"]
                        parseStateMessage currentRes.ParseState
                        Heading.h4 [] [str "Download your results"]
                        hr[]
                        downloadView model dispatch
                    | InvalidCharacters _ | FilteredEmptySequence | EmptySequence | InternalServerError ->
                        br []
                        Heading.h4 [] [str currentRes.Header]
                        hr []
                        Heading.h4 [] [str "RunInfo"]
                        parseStateMessage currentRes.ParseState
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
            let hasSingleResult, hasMultiResult =
                Model.hasSingleResult model,
                Model.hasMultiResult model

            match model.SelectedComputationMode with
            | Legacy ->
                if model.SeqMode = Single && hasSingleResult then
                    match model.SingleSequenceResultLegacy with
                    |Some res -> yield Legacy.singleResult model dispatch res
                    |None -> ()
                elif model.SeqMode = File && hasMultiResult then
                    match model.FastaFileInputResultLegacy with
                    |Some res -> yield Legacy.multipleResults model dispatch res
                    |None -> ()
            | IMLP ->
                if model.SeqMode = Single && hasSingleResult then
                    match model.SingleSequenceResultIMLP with
                    |Some res -> yield IMLP.singleResult model dispatch res
                    |None -> ()
                elif model.SeqMode = File && hasMultiResult then
                    match model.FastaFileInputResultIMLP with
                    |Some res -> yield IMLP.multipleResults model dispatch res
                    |None -> ()
        ]
    
module Client

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

open AppModel
open StateHandling

open GenericViewComponents
open ResultViews

//let progressDetails (show:bool) (model : Model)  (dispatch: Msg -> unit)  =
//    div [] [
//        if model.HasJobRunning then 
//            yield Button.button [Button.OnClick (fun _ -> ShowProgressDetails |> dispatch)] [if show then yield str "Hide progress details" else yield str "Show progress details"]
//            yield
//                Container.container [if (not show) then yield Container.Props [Props.Style [Props.Display DisplayOptions.None]]] [
//                    Table.table [][
//                        thead [] [
//                            th [] [
//                                str "Protein entry header"
//                            ]
//                            th [] [
//                                str "Status"
//                                ]
//                        ]
//                        tbody [] [
//                           yield!
//                            model.FastaFileInput
//                            |> Array.mapi
//                                (fun i entry ->
//                                    let rowEntry =
//                                        [
//                                            td [] [
//                                                str (entry.Split('\n') |> Array.head)
//                                            ]
//                                        ]
//                                    if i < model.FileProcessIndex then
//                                        tr [] [
//                                            yield! rowEntry
//                                            yield
//                                                td [] [
//                                                    str "Done"
//                                                    Icon.icon [] [Fa.i [Fa.Solid.Check] []]
//                                                ]
//                                        ]
//                                    elif i = model.FileProcessIndex then
//                                        tr [] [
//                                            yield! rowEntry
//                                            yield
//                                                td [] [
//                                                    str "Processing"
//                                                    Icon.icon [] [Fa.i [Fa.Solid.Spinner] []]
//                                                ]
//                                        ]
//                                    else
//                                        tr [] [
//                                            yield! rowEntry
//                                            yield
//                                                td [] [
//                                                    str "Queued"
//                                                    Icon.icon [] [Fa.i [Fa.Solid.TruckLoading] []]
//                                                ]
//                                        ]
//                                )
//                        ]
//                    ]
//                ]
//    ]


//let targetPModelSelector (isTargetSelector: bool) (model : Model) (dispatch : Msg -> unit) =
    
//    let selectionClassPlant = 
//        match model.SelectedTargetPModel with
//        | Plant when isTargetSelector -> "is-selected-model"
//        | _     -> ""

//    let selectionClassNonPlant = 
//        match model.SelectedTargetPModel with
//        | NonPlant when isTargetSelector-> "is-selected-model"
//        | _         -> ""

//    Columns.columns [] [
//        Column.column [Column.CustomClass "has-text-centered"] [
//            Heading.h6 [] [str "Plant model"]
//            Image.image [
//                Image.Option.Is128x128;
//                Image.CustomClass (sprintf "is-centered is-inline-block is-rounded %s" selectionClassPlant)] [
//                img [
//                    Props.Src "/Images/PlantModel.png";
//                    Props.OnClick (fun ev -> if not (model.SeqMode = NotSelected) then TargetPModelSelection TargetPModel.Plant |> dispatch)
//                ]
//            ]
//        ]
//        Column.column [Column.CustomClass "has-text-centered"] [
//            Heading.h6 [] [str "Non-Plant model"]
//            Image.image [
//                Image.Option.Is128x128;
//                Image.CustomClass (sprintf "is-centered is-inline-block is-rounded %s" selectionClassNonPlant)] [
//                img [
//                    Props.Src "/Images/YeastModel.png"
//                    Props.OnClick (fun ev -> if not (model.SeqMode = NotSelected) then TargetPModelSelection TargetPModel.NonPlant |> dispatch)
//                ]
//            ]
//        ]
//    ]

let view (model : Model) (dispatch : Msg -> unit) =
    div [] [
        navbar model dispatch
        errorModal true model dispatch
        eulaModal model dispatch
        displayHelpModal model dispatch
        hero model dispatch
        inputSelection model dispatch
        ResultViews.ViewComponents.resultHeading model dispatch 
        ResultViews.CompositeViews.resultSection model dispatch
        Footer.footer [] [
            Content.content [] [
                str "This service is developed and maintained by the "
                a [Props.Href "https://csb.bio.uni-kl.de/"] [str "Computational Systems Biology department "]
                str "of the TU Kaiserslautern, Germany."
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

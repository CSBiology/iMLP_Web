module AppModel

open Shared

type Mode =
|NotSelected
|Single
|File

type DisplayHelp =
|NoHelp
|TechnicalScientificDetails
|Contact
|HowToUse
|InputFormat

type PlotMode =
|Propensity
|TargetPScore

module Server =

    open Shared
    open Fable.Remoting.Client

    let targetPApi : ITargetPApi = 
        Remoting.createApi()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.buildProxy<ITargetPApi>

// The model holds data that you want to keep track of while the application is running
type Model = { 
    SessionGuid                 :   System.Guid
    EULAAccepted                :   bool
    EULAModalVisible            :   bool
    BurgerVisible               :   bool
    SelectedComputationMode     :   ComputationMode
    SelectedOrganismModel       :   OrganismModel
    SingleSequence              :   string
    SingleSequenceResultLegacy  :   LegacyResult Option
    SingleSequenceResultIMLP    :   IMLPResult Option
    FastaFileInput              :   string []
    FastaFileInputName          :   string
    FastaFileInputResultLegacy  :   (LegacyResult array) Option
    FastaFileInputResultIMLP    :   (IMLPResult array) Option
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
    ShowErrorStack              :   bool
    PlotMode                    :   PlotMode
} with

    static member validateInputState (model:Model) =
        match model.SeqMode with
        |Single -> 
            match model.SingleSequence with
            |"" -> false,"No data provided"
            | _ ->
                match model.HasValidFasta with
                | false -> false, "Fasta is invalid"
                | _ ->  if model.EULAAccepted then
                            true,"Start legacy computation"
                        else
                            true,"Start computation"
        |File -> 
            match model.FastaFileInput with
            |[||] -> false,"No data provided"
            | x when x.Length > 1000 ->
                false, "Too many sequences (>1000)."
            | _ ->
                match model.HasValidFasta with
                | false -> false, "Fasta is invalid"
                | _ ->  if model.EULAAccepted then
                            true,"Start legacy computation"
                        else
                            true,"Start computation"
        |_ -> false,"No data provided"

    static member getMultiResultLength (model:Model) =
        match model.SelectedComputationMode with
        | Legacy -> model.FastaFileInputResultLegacy.Value.Length
        | IMLP -> model.FastaFileInputResultIMLP.Value.Length

    static member hasMultiResult (model:Model) =
        match model.SelectedComputationMode with
        | Legacy -> model.FastaFileInputResultLegacy.IsSome
        | IMLP -> model.FastaFileInputResultIMLP.IsSome

    static member hasSingleResult (model:Model) =
        match model.SelectedComputationMode with
        | Legacy -> model.SingleSequenceResultLegacy.IsSome
        | IMLP -> model.SingleSequenceResultIMLP.IsSome

    static member tryGetSingleSequenceScoresOfInterest (model:Model) =
        match (model.SelectedComputationMode, model.SeqMode) with
        | (IMLP, Single)    -> model.SingleSequenceResultIMLP |> Option.map (fun x -> x.PropensityScores)
        | (Legacy, Single)  ->
            match model.PlotMode with
            | PlotMode.Propensity   ->  model.SingleSequenceResultLegacy |> Option.map (fun x -> x.PropensityScores)
            | PlotMode.TargetPScore ->  model.SingleSequenceResultLegacy |> Option.map (fun x -> x.RawTargetPScores)
        | _ -> None

let initialModel = {
    SessionGuid                 =   System.Guid.NewGuid()
    EULAAccepted                =   false
    EULAModalVisible            =   false
    BurgerVisible               =   false
    SelectedComputationMode     =   ComputationMode.IMLP
    SelectedOrganismModel       =   NonPlant
    SingleSequence              =   ""
    SingleSequenceResultLegacy  =   None
    SingleSequenceResultIMLP    =   None
    FastaFileInput              =   [||]
    FastaFileInputName          =   "No file selected"
    FastaFileInputResultLegacy  =   None
    FastaFileInputResultIMLP    =   None
    SeqMode                     =   Mode.NotSelected
    ShowResults                 =   false
    ResultHeadingIsSticky       =   false
    CurrentResultViewIndex      =   0
    DownloadReady               =   false
    DownloadFileName            =   "IMTS_prediction_results.txt"
    FileProcessIndex            =   0
    HasValidFasta               =   true
    InvalidFastaChars           =   []
    HasJobRunning               =   false
    ShowProgressDetails         =   false
    InformationSectionDisplay   =   NoHelp
    HasError                    =   false
    ErrorState                  =   None
    ShowErrorStack              =   false
    PlotMode                    =   Propensity
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
| Reset
| ToggleBurger
| EULAAcceptedChange
| ShowEulaModal                     of bool
| SeqModeSelection                  of Mode
| OrganismModelSelection            of OrganismModel
| FastaUploadInput                  of string*string
| SingleSequenceInput               of string
// Upload a single sequence and execute serverside computation based on computation mode
| SingleSequenceRequest             of ComputationMode
| SingleSequenceResponseLegacy      of Result<LegacyResult,exn>
| SingleSequenceResponseIMLP        of Result<IMLPResult,exn>
// Upload a fasta file and execute serverside computation based on computation mode
| FastaUploadRequest                of ComputationMode
| FastaUploadResponseLegacy         of Result<LegacyResult,exn>
| FastaUploadResponseIMLP           of Result<IMLPResult,exn>
| FileProcessingDone
| ShowPlot                          of int 
| PrepareDownloadCSV
| DownloadResponse                  of Result<unit,exn>
| DownloadFileNameChange            of string
| ShowProgressDetails
| ToggleResultHeadingSticky         of bool
| ChangeViewIndex                   of int
| ChangeHelpDisplay                 of DisplayHelp
| FastaValidation                   of Result<string,char list>
| GenericError                      of exn
| ChangeErrorStateVisibility
| ChangePlotMode                    of PlotMode
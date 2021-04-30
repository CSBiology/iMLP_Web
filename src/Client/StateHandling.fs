module StateHandling

open Elmish
open Shared
//open Fulma.FontAwesome
open Browser.Dom
open AppModel


let validateFastaText (fsa:string) =
    let allSeqs =
        fsa
        |> fun x -> x.Replace("\r\n","\n")
        |> fun x -> x.Split('\n')
        |> Array.filter (fun x -> not (x.StartsWith(">")))
    let validSet =
        set [
            'A';'C';'D';'E';'F';'G';'H';'I';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'Y';'X';'J';'Z';'B';'*';'-';'\n';'\r'
            'a';'c';'d';'e';'f';'g';'h';'i';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'y';'x';'j';'z';'b'
            ]

    let invalidChars =
        allSeqs
        |> Array.map (String.filter (fun x -> not (validSet.Contains(x))))
        |> Array.filter (fun s -> s.Length > 0)
        |> String.concat ""
        |> fun x -> [for c in x do yield c]
        |> List.distinct

    let isValid = invalidChars.Length = 0

    if isValid then Ok fsa else Error (invalidChars)

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    initialModel, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | Reset -> init ()
    | EULAAcceptedChange ->
        let isAccepted = not currentModel.EULAAccepted
        let computationMode = if isAccepted then ComputationMode.Legacy else ComputationMode.IMLP
        let updatedModel = {
            currentModel with
                EULAAccepted = isAccepted
                SelectedComputationMode = computationMode
            }
        updatedModel,Cmd.none
    | ShowEulaModal isVisible ->
        let updatedModel = {currentModel with EULAModalVisible = isVisible}
        updatedModel,Cmd.none
    | GenericError exn ->
        let updatedModel = {currentModel with HasError = true; ErrorState=Some exn}
        updatedModel,Cmd.none
    | ChangeErrorStateVisibility ->
        let updatedModel = {currentModel with ShowErrorStack = not currentModel.ShowErrorStack}
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

    | OrganismModelSelection m ->
        let updatedModel = {currentModel with SelectedOrganismModel = m}
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
            

    | SingleSequenceRequest mode ->

        match mode with
        | Legacy  ->
        
            let updatedModel = {
                currentModel with
                    DownloadReady = false;
                    HasJobRunning = true
                    }
            let requestCmd = 
                Cmd.OfAsync.either
                    Server.targetPApi.SingleSequenceRequestLegacy
                    (currentModel.SelectedOrganismModel, currentModel.SingleSequence)
                    (Ok >> SingleSequenceResponseLegacy)
                    (Error >> SingleSequenceResponseLegacy)
            updatedModel,requestCmd

        | IMLP ->

            let updatedModel = {
                currentModel with
                    DownloadReady = false;
                    HasJobRunning = true
                    }
            let requestCmd = 
                Cmd.OfAsync.either
                    Server.targetPApi.SingleSequenceRequestIMLP
                    (currentModel.SelectedOrganismModel, currentModel.SingleSequence)
                    (Ok >> SingleSequenceResponseIMLP)
                    (Error >> SingleSequenceResponseIMLP)
            updatedModel,requestCmd

    | FastaUploadRequest mode ->

        match mode with
        | Legacy -> 
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
                        Server.targetPApi.SingleSequenceRequestLegacy
                        (currentModel.SelectedOrganismModel, currentModel.FastaFileInput.[processIndex])
                        (Ok >> FastaUploadResponseLegacy )
                        (Error >> FastaUploadResponseLegacy )
                else
                    Cmd.ofMsg FileProcessingDone
                    
            updatedModel,requestCmd

        | IMLP ->
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
                        Server.targetPApi.SingleSequenceRequestIMLP
                        (currentModel.SelectedOrganismModel, currentModel.FastaFileInput.[processIndex])
                        (Ok >> FastaUploadResponseIMLP )
                        (Error >> FastaUploadResponseIMLP )
                else
                    Cmd.ofMsg FileProcessingDone
                    
            updatedModel,requestCmd

    | SingleSequenceResponseLegacy (Ok res) ->
        let updatedModel = {
            currentModel with 
                SingleSequenceResultLegacy = Some res
                ShowResults = true
                HasJobRunning = false
                }
        updatedModel,Cmd.none

    | SingleSequenceResponseLegacy (Error res) ->
        console.log(res)
        currentModel,Cmd.ofMsg (GenericError res)

    | SingleSequenceResponseIMLP (Ok res) ->
        let updatedModel = {
            currentModel with 
                SingleSequenceResultIMLP = Some res
                ShowResults = true
                HasJobRunning = false
                }
        updatedModel,Cmd.none

    | SingleSequenceResponseIMLP (Error res) ->
        console.log(res)
        currentModel,Cmd.ofMsg (GenericError res)

    | FastaUploadResponseLegacy (Ok res) ->
        let updatedModel = {
            currentModel with 
                FastaFileInputResultLegacy =
                    match currentModel.FastaFileInputResultLegacy with
                    |Some cres  -> Some [|yield! cres; yield res|]
                    |None       -> Some [|res|]
                ShowResults = true
                FileProcessIndex = currentModel.FileProcessIndex + 1
                }
        updatedModel,Cmd.ofMsg (FastaUploadRequest ComputationMode.Legacy)

    | FastaUploadResponseLegacy (Error res) ->
        //TODO: handle request error!
        console.log(res)
        currentModel,Cmd.ofMsg (GenericError res)

    | FastaUploadResponseIMLP (Ok res) ->
        let updatedModel = {
            currentModel with 
                FastaFileInputResultIMLP =
                    match currentModel.FastaFileInputResultIMLP with
                    |Some cres  -> Some [|yield! cres; yield res|]
                    |None       -> Some [|res|]
                ShowResults = true
                FileProcessIndex = currentModel.FileProcessIndex + 1
                }
        updatedModel,Cmd.ofMsg (FastaUploadRequest ComputationMode.IMLP)

    | FastaUploadResponseIMLP (Error res) ->
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
        match (currentModel.SeqMode, currentModel.SelectedComputationMode) with
        |(Single, Legacy) ->
            match currentModel.SingleSequenceResultLegacy with
            |Some res ->
                let requestCmd = 
                    Cmd.OfAsync.either
                        Server.targetPApi.DownloadRequestSingleLegacy
                        (res,currentModel.SessionGuid)
                        (Ok >> DownloadResponse)
                        (Error >> DownloadResponse)
                currentModel,requestCmd
            |_ -> currentModel,Cmd.none
        |(Single, IMLP) ->
            match currentModel.SingleSequenceResultIMLP with
            |Some res ->
                let requestCmd = 
                    Cmd.OfAsync.either
                        Server.targetPApi.DownloadRequestSingleIMLP
                        (res,currentModel.SessionGuid)
                        (Ok >> DownloadResponse)
                        (Error >> DownloadResponse)
                currentModel,requestCmd
            |_ -> currentModel,Cmd.none
        |(File, Legacy) ->
            match currentModel.FastaFileInputResultLegacy with
                |Some res ->
                    let requestCmd = 
                        Cmd.OfAsync.either
                            Server.targetPApi.DownloadRequestMultipleLegacy
                            (res,currentModel.SessionGuid)
                            (Ok >> DownloadResponse)
                            (Error >> DownloadResponse)
                    currentModel,requestCmd
                |_ -> currentModel,Cmd.none
        |(File, IMLP) ->
            match currentModel.FastaFileInputResultIMLP with
                |Some res ->
                    let requestCmd = 
                        Cmd.OfAsync.either
                            Server.targetPApi.DownloadRequestMultipleIMLP
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
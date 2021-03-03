namespace Shared

type Counter = int

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type TargetPModel =
|NoModel
|Plant
|NonPlant

//Start and end indices 0 based!
type IMTSL = {
    StartIndex: int
    EndIndex: int
    ScoreSum : float
}

type TargetPResult = {
    Header              : string
    Sequence            : string
    Scores              : float array
    SmoothedScores      : float array
    Propensity          : float array
    PredictedIMTSL      : IMTSL array
    PropensityPlotHtml  : string
    ScorePlotHtml       : string
}

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html

type ITargetPApi = {
    SingleSequenceRequest : TargetPModel -> string -> Async<TargetPResult>
    //FastaFileRequest : TargetPModel -> string -> Async<TargetPResult array>
    DownloadRequestSingle: TargetPResult*System.Guid -> Async<unit>
    DownloadRequestMultiple: TargetPResult array * System.Guid -> Async<unit>
}
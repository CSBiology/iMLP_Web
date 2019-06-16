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

type TargetPResult = {
    Header      : string
    Sequence    : string
    Scores      : float array
    PlotHtml    : string
}

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html

type ITargetPApi = {
    SingleSequenceRequest : TargetPModel -> string -> Async<TargetPResult>
    FastaFileRequest : TargetPModel -> string -> Async<TargetPResult array>
    DownloadRequestSingle: TargetPResult*System.Guid -> Async<unit>
    DownloadRequestMultiple: TargetPResult array * System.Guid -> Async<unit>
}
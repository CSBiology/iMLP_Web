namespace Shared

type Counter = int

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type OrganismModel =
|Plant
|NonPlant

type ComputationMode =
| Legacy
| IMLP


type LegacyResult = {
    Header                  : string
    Sequence                : string
    RawTargetPScores        : float array
    SmoothedTargetPScores   : float array
    PropensityScores        : float array
    PropensityPlotHtml      : string
    ScorePlotHtml           : string
}

type IMLPResult = {
    Header              : string
    Sequence            : string
    PropensityScores    : float array
    PropensityPlotHtml  : string
}

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html

type ITargetPApi = {

    // Legacy model
    SingleSequenceRequestLegacy : OrganismModel*string -> Async<LegacyResult>
    DownloadRequestSingleLegacy: LegacyResult*System.Guid -> Async<unit>
    DownloadRequestMultipleLegacy: LegacyResult array * System.Guid -> Async<unit>

    //iMLP model
    SingleSequenceRequestIMLP : OrganismModel*string -> Async<IMLPResult>
    DownloadRequestSingleIMLP: IMLPResult*System.Guid -> Async<unit>
    DownloadRequestMultipleIMLP: IMLPResult array * System.Guid -> Async<unit>
}
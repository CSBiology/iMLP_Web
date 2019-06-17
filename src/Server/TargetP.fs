module TargetPServer

open BioFSharp.BioTools
open BioFSharp.BioTools.TargetP
open System.IO
open BioContainer
open FSharpAux
open FSharpAux.IO
open FSharpAux.IO.SchemaReader.Attribute

let runWithMountAsync (bcContext:BioContainer.BcContext) (opt:TargetpParams) (inputFile:string) =
    let cPath = MountInfo.containerPathOf bcContext.Mount inputFile
    let tp = "targetp"::TargetpParams.makeCmd opt
    async {
        let! targepResult =
            BioContainer.execReturnAsync bcContext (tp@[cPath])

        let skipLines             = 1
        let skipLinesBeforeHeader = 6 //6
        let schemaMode = SchemaReader.Csv.Fill
        let csvReader = SchemaReader.Csv.CsvReader<TargetpItem>(SchemaMode=schemaMode)
        
        return csvReader.ReadFromString(targepResult,'\t',true,skipLines, skipLinesBeforeHeader)
 
    }

let runWithMount (bcContext:BioContainer.BcContext) (opt:TargetpParams) (inputFile:string) =
    runWithMountAsync bcContext opt inputFile
    |> Async.RunSynchronously
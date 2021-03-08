module CNTKServer


open System
open BioFSharp
open BioFSharp.BioArray
open AminoAcids
open BioFSharp.Formula
open FSharpAux
open CNTK
open System.Collections.Generic

type targetPOut = {
    QID         : int
    AA          : AminoAcid
    AAIdx       : int
    TargetPScore: float 
    }

let aminoAcidSetStandard =
    [
        AminoAcid.Ala
        AminoAcid.Cys
        AminoAcid.Asp
        AminoAcid.Glu
        AminoAcid.Phe
        AminoAcid.Gly
        AminoAcid.His
        AminoAcid.Ile
        AminoAcid.Lys
        AminoAcid.Leu
        AminoAcid.Met
        AminoAcid.Asn
        //AminoAcid.Pyl
        AminoAcid.Pro
        AminoAcid.Gln
        AminoAcid.Arg
        AminoAcid.Ser
        AminoAcid.Thr
        //AminoAcid.Sel
        AminoAcid.Val
        AminoAcid.Trp
        AminoAcid.Tyr
    ]  

let aminoAcidToVectorIdx = 
    aminoAcidSetStandard        
    |> List.mapi (fun i x -> x,i)
    |> Map.ofList
      
let modelP = @"IMTS.model"
let device = DeviceDescriptor.CPUDevice

let PeptidePredictor : Function = 
    Function.Load(modelP,device)

let x' = 
    PeptidePredictor.Parameters()
    |> Seq.toList
    |> fun x -> x.[x.Length-1].Shape

///////////Input 
let inputVar: Variable = 
    PeptidePredictor.Arguments.Item 0

let inputShape = inputVar.Shape

let CNCRepresentationOf (protein:targetPOut []) =
    /// 
    let rowIndices            = new ResizeArray<int>()
    /// new word // should in my case be 1
    let colStarts             = new ResizeArray<int>()
    let nonZeroValues         = new ResizeArray<float32>()
    protein
    |> Array.iteri (fun i x -> 
                    nonZeroValues.Add (float32 1.)
                    rowIndices.Add x.AAIdx
                    colStarts.Add i
                    )
    colStarts.Add protein.Length
    let rowIndices    = rowIndices |> Array.ofSeq
    let nonZeroValues = nonZeroValues |> Array.ofSeq
    let colStarts = colStarts |> Array.ofSeq
    Value.CreateSequence<float32>(20,protein.Length,colStarts,rowIndices,nonZeroValues,device)

let predict featureData = 
    //inputShape    
    let inputValues = CNCRepresentationOf featureData

    let inputMap = new Dictionary<Variable,Value>()
    inputMap.Add(inputVar,inputValues)

    let outputVar : Variable = PeptidePredictor.Output

    let outputMap = new Dictionary<Variable,Value>()
    outputMap.Add(outputVar,null)

    PeptidePredictor.Evaluate(inputMap,outputMap,device)

    let outputValues = outputMap.[outputVar]

    let preds = 
        outputValues.GetDenseData<float32>(outputVar)
        |> Seq.concat
        |> Array.ofSeq


    let xF,yF = 
        Array.mapi (fun (i) x -> i,x.TargetPScore) (featureData) 
        |> Array.unzip

    let xP,yP = 
        Array.mapi (fun (i) x -> i,float x) (preds) 
        |> Array.unzip
   
    yF, yP

let bioSeqToInput (input: BioSeq.BioSeq<AminoAcids.AminoAcid>) :targetPOut[] = 
        input 
        |> Seq.choose (fun x -> 
            try
            let idx = aminoAcidToVectorIdx.[x]
            {
            QID          = 0
            AA           = x 
            AAIdx        = idx 
            TargetPScore = nan
            }
            |> Some
            with
            | _-> None
            )
        |> Array.ofSeq

let predictFinal (sequence:string) = 
    let bsequence = BioSeq.ofAminoAcidString sequence
    let _, predictedTraces = predict (bioSeqToInput bsequence) 
    predictedTraces
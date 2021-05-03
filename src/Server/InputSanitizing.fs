module InputSanitizing

open System
open System.IO
open FSharpAux
open FSharpAux.IO
open BioFSharp
open BioFSharp.IO
open Shared

let validSet =
    set [
        'A';'C';'D';'E';'F';'G';'H';'I';'K';'L';'M';'N';'P';'Q';'R';'S';'T';'U';'V';'W';'Y';'X';'Z';'B';'O';'J';'*';'-'
        'a';'c';'d';'e';'f';'g';'h';'i';'k';'l';'m';'n';'p';'q';'r';'s';'t';'u';'v';'w';'y';'x';'z';'b';'o';'j';'*';'-'
        ]


let sanitizeInputSequence (inputSequence:string) : ParseState =

    let charArr =
        inputSequence
        |> String.toCharArray

    if inputSequence.Length > 0 then

        let containsInvalidChars =
            charArr
            |> Array.exists (fun c -> not (validSet.Contains(c)))

        if containsInvalidChars then

            let invalidChars =
                charArr
                |> Array.filter (fun c -> not (validSet.Contains(c)))

            InvalidCharacters invalidChars

        else
            let containsGapTerOJ =
                charArr
                |> Array.exists (fun c -> (c = '*' || c = '-' || c = 'O' || c = 'J' || c = 'o' || c = 'j'))

            let isShort = charArr.Length < 21

            match (isShort, containsGapTerOJ) with
            | (true,true) ->

                printfn "input is short and contains gap/ter/o/j"

                let filtered =
                    charArr
                    |> Array.filter (fun c -> not (c = '*' || c = '-' || c = 'O' || c = 'J' || c = 'o' || c = 'j'))
                    |> String.fromCharArray

                if filtered.Length > 0 then
                    FilteredShortSequence (
                        charArr
                        |> Array.filter (fun c -> not (c = '*' || c = '-' || c = 'O' || c = 'J' || c = 'o' || c = 'j'))
                        |> String.fromCharArray
                    )

                else
                    FilteredEmptySequence

            | (false, true) ->

                printfn "input is not short but contains gap/ter"

                let filtered =
                    charArr
                    |> Array.filter (fun c -> not (c = '*' || c = '-' || c = 'O' || c = 'J' || c = 'o' || c = 'j'))
                    |> String.fromCharArray

                if filtered.Length = 0 then
                    FilteredEmptySequence

                elif filtered.Length < 21 then
                    FilteredShortSequence (
                        charArr
                        |> Array.filter (fun c -> not (c = '*' || c = '-' || c = 'O' || c = 'J' || c = 'o' || c = 'j'))
                        |> String.fromCharArray
                    )

                else
                    ContainsGapTerOJ (
                        charArr
                        |> Array.filter (fun c -> not (c = '*' || c = '-' || c = 'O' || c = 'J' || c = 'o' || c = 'j'))
                        |> String.fromCharArray
                    )


            | (true,false) ->
                ShortSequence (charArr |> String.fromCharArray)

            | (false, false) ->
                Success (charArr |> String.fromCharArray)

    else
        EmptySequence

let extractHeaderAndSequence (raw:string) =
    let splt =
        raw
            .Replace("\r\n","\n")
            |> String.split '\n'
    if splt.[0].StartsWith(">") then
        splt.[0], Array.tail splt
    else
        ">No Header Provided", splt


module JSInterop

open Fable.Core.JsInterop
open Fable.Core.DynamicExtensions
open Fable.Core
open Browser.Dom
open Fable.React
open Fable.React.Props
open Browser.Types

//open Fulma.FontAwesome

module FileInputHelper =

    //!!===================================================!!
    //Credit goes to https://github.com/GuuD/fable-file-input
    //!!===================================================!!

    type FileInfo<'t> = 
        { Name: string; MIME: string; Data: 't }

    [<Emit("new Promise($0)")>]
    let createPromise (executor: ('t -> unit) -> (exn -> unit) -> unit): JS.Promise<'t> = jsNative

    [<Emit("$1.then($0)")>]
    let consumePromise (callback: 't->unit) (_promise: JS.Promise<'t>): unit = jsNative
    let private readInternal<'t> (readMethod: string) (blob: Browser.Types.Blob) = 
        createPromise(fun resolve reject -> 
            try
                let reader = Browser.Dom.FileReader.Create()
                reader.onload <- (fun _ -> reader.result |> unbox<'t> |> resolve)
                reader.[readMethod].Invoke(blob) |> ignore
            with 
            | e -> e |> reject
        )
    let readAsText blob: JS.Promise<string> = 
        readInternal "readAsText" blob
    let readAsDataURL blob: JS.Promise<string> = 
        readInternal "readAsDataURL" blob
    let readAsArrayBuffer blob: JS.Promise<JS.ArrayBuffer> = 
        readInternal "readAsArrayBuffer" blob
    module React = 
        open Fable.React
        open Props
        let extract f list = 
            let rec seek traversed = function
                | h::t ->
                    match f h with
                    | Some h' -> Some h', (List.rev traversed)@t
                    | _ -> seek (h::traversed) t
                | [] -> None, (List.rev traversed)
            seek [] list

        type FileCallback = 
            | OnFileBytesReceived of (FileInfo<JS.ArrayBuffer> -> unit)
            | OnDataUrlReceived of (FileInfo<string> -> unit)
            | OnTextReceived of (FileInfo<string> -> unit)
            interface Props.IHTMLProp

        let inline singleFileInput (props: Props.IHTMLProp list) = 
            let existingChangeHandler, otherProps = 
                props |> extract (function 
                    | :? DOMAttr as prop -> 
                        match prop with
                        | OnChange callback -> Some callback
                        | _ -> None
                    | _ -> None)
            let loadCallback, withoutCallbacks = otherProps |> extract (function | :? FileCallback as fc -> Some fc | _ -> None)   
            let changeHandler (e : Event) = 
                let files: FileList = !!e.target.["files"]
                if files.length > 0 then
                    let file = files.[0]
                    match loadCallback with
                    | Some(OnFileBytesReceived r) -> 
                        readAsArrayBuffer file
                        |> consumePromise (fun bytes -> r { Name = file.name; MIME = file.``type``; Data = bytes })
                    | Some(OnDataUrlReceived r) ->
                        readAsDataURL file
                        |> consumePromise (fun data -> r { Name = file.name; MIME = file.``type``; Data = data } )
                    | Some(OnTextReceived r) ->
                        readAsText file
                        |> consumePromise (fun data -> r { Name = file.name; MIME = file.``type``; Data = data } )
                    | _ -> console.warn("You probably need to attach callback to the file input field")                
                match existingChangeHandler with
                | Some h -> h e
                | _ -> ()
            input ([OnChange changeHandler; Type "file" ]@withoutCallbacks)


let gradientColorTable = [|
    "#FAEE05"
    "#FAEE05"
    "#F8E109"
    "#F7D40E"
    "#F5C813"
    "#F4BB18"
    "#F2AF1D"
    "#F1A222"
    "#EF9627"
    "#EE892C"
    "#ED7D31"
|]

type CustomHTMLAttr = 
    | [<CompiledName("data-dismiss")>]  DataDismiss     of string
    | [<CompiledName("aria-label")>]    AriaLabel       of string
    | [<CompiledName("aria-hidden")>]   AriaHidden      of bool
    | [<CompiledName("aria-controls")>] AriaControls    of string
    | [<CompiledName("aria-haspopup")>] AriaHasPopup    of bool
    interface IHTMLProp 
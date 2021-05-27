module GenericViewComponents

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
open Fable.FontAwesome

open Shared
open JSInterop
open FileInputHelper
open FileInputHelper.React

open AppModel
open StateHandling

let block children = div [Class "block"] children 

let navbar (model : Model) (dispatch : Msg -> unit) =
    let currentDisp = model.InformationSectionDisplay
    Navbar.navbar [Navbar.IsFixedTop; Navbar.CustomClass "is-dark csbNav"; Navbar.Props [Props.Role "navigation"; AriaLabel "main navigation" ]] [
        Navbar.Brand.a [] [
            Navbar.Item.a [Navbar.Item.Props [Props.Href "https://csb.bio.uni-kl.de/"]] [
                img [Props.Src "../Images/Logo.png"]
            ]
            Navbar.burger   [   Navbar.Burger.IsActive model.BurgerVisible
                                Navbar.Burger.Props [
                                    Props.Role "button"
                                    AriaLabel "menu"
                                    Props.AriaExpanded false
                                    OnClick (fun e -> ToggleBurger |> dispatch)
                                ]
                            ] [
                span [AriaHidden true] []
                span [AriaHidden true] []
                span [AriaHidden true] []
            ]
        ]
        Navbar.menu [Navbar.Menu.Props [Id "navbarMenu"; Class (if model.BurgerVisible then "navbar-menu is-active" else "navbar-menu") ]] [
            Navbar.Start.div [] [
                Navbar.Item.a
                    [
                        Navbar.Item.Props [OnClick (fun _ -> ChangeHelpDisplay (if currentDisp = HowToUse then NoHelp else HowToUse) |> dispatch)]
                        Navbar.Item.IsActive (currentDisp = HowToUse)
                    ] [
                    str "How to use"
                ]
                Navbar.Item.a
                    [
                        Navbar.Item.Props [OnClick (fun _ -> ChangeHelpDisplay (if currentDisp = InputFormat then NoHelp else InputFormat) |> dispatch)]
                        Navbar.Item.IsActive (currentDisp = InputFormat)
                    ] [
                    str "Input format"

                ]
                Navbar.Item.a
                    [
                        Navbar.Item.Props [OnClick (fun _ -> ChangeHelpDisplay (if currentDisp = TechnicalScientificDetails then NoHelp else TechnicalScientificDetails) |> dispatch)]
                        Navbar.Item.IsActive (currentDisp = TechnicalScientificDetails)
                    ] [
                    str "Technical details"
                ]
            ]
            Navbar.End.div [] [
                Navbar.Item.a
                    [
                        Navbar.Item.Props [OnClick (fun _ -> ChangeHelpDisplay (if currentDisp = Contact then NoHelp else Contact) |> dispatch)]
                        Navbar.Item.IsActive (currentDisp = Contact)
                    ] [
                    str "Contact"
                ]
            ]
        ]
    ]

let eulaModal (model:Model) (dispatch:Msg -> unit) =
    Modal.modal [Modal.IsActive model.EULAModalVisible] [
        Modal.background [] []
        Modal.content [Props[Style[BackgroundColor "white"; Padding "1em 1em 1em 1em"; TextAlign TextAlignOptions.Justify; Width "80%"]]] [
            Heading.h5 [] [str "ACADEMIC SOFTWARE LICENSE AGREEMENT FOR END-USERS AT PUBLICLY FUNDED ACADEMIC, EDUCATION OR RESEARCH INSTITUTIONS FOR THE USE OF targetp and iMLP."]
            p   [] [str """By accepting this License Agreement you are consenting to be bound by and become a party to this agreement as the "Licensee". If you do not agree to all of the terms of this agreement, you must not click the Acceptance button, nor use the product, and you do not become a LICENSEE under this agreement."""]
            br  []
            p   [] [str """If you are not a member of a publicly funded Academic and/or Education and/or Research Institution you must obtain a commercial license. This software license agreement is entered into by and between Technische Universität Kaiserslautern (hereinafter "LICENSOR") and the "LICENSEE"."""]
            br  []
            p   [] [str """WHEREAS LICENSEE is a public funded Academic and/or Education and/or Research Institution."""]
            br  []
            p   [] [str """WHEREAS LICENSEE desires to acquire a free non-exclusive license to use the Software for internal research purposes only."""]
            br  []
            p   [] [str """NOW, THEREFORE, in consideration of the mutual promises and covenants contained herein, the parties agree as follows:"""]
            br  []
            Heading.h6 [] [str "1. Definitions"]
            p   [] [str """"Licensed Software" means the specific version targetp and iMLP pursuant to this Agreement. """]
            br  []
            Heading.h6 [] [str "2. License"]
            p   [] [str """Subject to the terms and conditions of this Agreement a non-exclusive, non-transferable License to use and copy the Licensed Software is made available free of charge for the LICENSEE which is a non-profit educational, academic and/or research institution. The License is only granted for personal and internal use in research only at one Site, where a Site is defined as a set of contiguous buildings in one location. The software will be used at only one location of LICENSEE. """]
            br  []
            p   [] [str """This license does not entitle Licensee to receive from LICENSOR copies of the Licensed software on disks, tapes or CD's, hard-copy documentation, technical support, telephone assistance, or enhancements or updates to the Licensed Software. """]
            br  []
            p   [] [str """The user and any research assistants, co-workers or other workers who may use the Software agree to not give the program to third parties or grant licenses on software, which include the Software, alone or integrated into other software, to third parties. Modification of the source code is prohibited without the prior written consent of LICENSOR. """]
            br  []
            Heading.h6 [] [str "3. Ownership "]
            p   [] [str """Except as expressly licensed in this Agreement, LICENSOR shall retain title to the Licensed Software, and any upgrades and modifications created by LICENSOR."""]
            br  []
            Heading.h6 [] [str "4. Consideration "]
            p   [] [str """In consideration for the license rights granted by LICENSOR, LICENSEE will obtain this academic license free of charge. """]
            br  []
            Heading.h6 [] [str "5. Copies"] 
            p   [] [str """ LICENSEE shall have the right to make copies of the Licensed Software for internal use at the Site and for back-up purposes under this Agreement, but agrees that all such copies shall contain the copyright notices and all other reasonable and appropriate proprietary markings or confidential legends that appear on the Licensed Software provided hereunder."""]
            br  []
            Heading.h6 [] [str "6. Support"] 
            p   [] [ str """LICENSOR shall have no obligation to offer support services to LICENSEE, and nothing contained herein shall be interpreted as to require LICENSOR to provide maintenance, installation services, version updates, debugging, consultation or end-user support of any kind. """]
            br  []
            Heading.h6 [] [str "7. Software Protection"] 
            p   [] [ str """LICENSEE acknowledges that the Licensed Software is proprietary The software code shall be treated as trade secrets and confidential information of LICENSOR, and LICENSEE agrees to use best efforts to hold the same in confidence. LICENSEE's obligation for confidentiality shall not extend to any information which is or becomes generally available to the public, is already known to or subsequently disclosed by third parties to LICENSEE and at its free disposal, or is independently developed by LICENSEE or its affiliates without the use of the confidential information disclosed by LICENSOR, or is required by law or legal process. """]
            br  []
            p   [] [ str """Except as other wise expressly permitted in this Agreement, Licensee my not (i) modify or create any derivative works of the Licensed Software or documentation, including customization, translation or localization; (ii) decompile, disassemble, reverse engineer, or otherwise attempt to derive the source code for the Product; (iii) redistribute, encumber, sell, rent, lease, sublicense, or otherwise transfer rights to the Licensed Software; (iv) remove or alter any trademark, logo, copyright or other proprietary notices, legends, symbols or labels in the Product; or (v) publish any results of benchmark tests run on the Product to a third party without LICENSOR's prior written consent. """]
            br  []
            Heading.h6 [] [str "8. Representations of LICENSOR to LICENSEE"] 
            p   [] [ str """ LICENSOR represents to LICENSEE that (i) LICENSOR has the right to grant the License and to enter into this agreement, (ii) that, to the best of LICENSOR's knowledge, the Licensed software does not infringe any patent, copyright or trade secrets of any third party, provided however that such representation and warranty shall not apply to any addition to, or modifications or adaptation of, the Licensed Software made by LICENSEE and (iii) LICENSOR undertakes to use best efforts to cooperate with and assist LICENSEE, at LICENSEE's expense, in defending itself against any action based on the alleged infringement of any third party patent, copyright or trade secret rights resulting from or relating to the use or licensing of the Licensed Software by LICENSEE. """]
            br  []
            Heading.h6 [] [str "9. Indemnity and Disclaimer of Warranties"] 
            p   [] [ str """Except as expressly set forth in this agreement, LICENSOR makes no representations or warranties, express or implied. """]
            br  []
            p   [] [ str """The product is provided free of charge, and, therefore, on an "as is" basis, without warranty of any kind, express or implied, including without limitation the warranties that it is free of defects, virus free, able to operate on an uninterrupted basis, merchantable, fit for a particular purpose or non-interfering. The entire risk as to the quality and performance of the Licensed Software is borne by LICENSEE."""]
            br  []
            p   [] [ str """By way of example, but not limitation, LICENSOR makes no representations or warranties of merchantability or fitness for any particular application or, except as set forth in paragraph 8, that the use of the Software will not infringe any patents, copyrights or trademarks or other rights of third parties. The entire risk as to the quality and performance of the product is borne by LICENSEE. LICENSOR shall not be liable for any liability or damages with respect to any claim by LICENSEE or any third party on account of, or arising from the license or use of the Software."""]
            br  []
            p   [] [ str """Should the Licensed Software prove defective in any respect, LICENSEE and not LICENSOR or its affiliates should assume the entire cost of any service and repair. This disclaimer of warranty constitutes an essential part of this agreement. No use of the licensed product is authorized hereunder except under this disclaimer."""]
            br  []
            p   [] [ str """In no event will LICENSOR or its affiliates be liable for any indirect, special, incidental or consequential damages arising out of the use of or inability to use the product, including, without limitation, damages for lost profits, loss of goodwill, work stoppage, computer failure or malfunction, or any and all other commercial damages or losses, even if advised of the possibility thereof, and regardless of the legal or equitable theory (contract, tort or otherwise) upon which the claim is based. """]
            br  []
            Heading.h6 [] [str "10. Promotional Advertising & References"]
            p   [] [ str """LICENSEE may not use the name of the Licensed Software in its promotional advertising, product literature, and other similar promotional materials to be disseminated to the public or any portion thereof. LICENSEE agrees not to identify LICENSOR in any promotional advertising or other promotional materials to be disseminated to the public, or any portion thereof without LICENSOR's prior written consent."""]
            br  []
            Heading.h6 [] [str "11. Term "]
            p   [] [ str """This Agreement and the license rights granted herein shall become effective as of the date this Agreement is executed by both parties and shall be perpetual unless terminated in accordance with this Section."""]
            br  []
            p   [] [ str """LICENSOR may terminate this Agreement at any time."""]
            br  []
            p   [] [ str """Either party may terminate this Agreement at any time effective upon the other party's breach of any agreement, covenant, or representation made in this Agreement, such breach remaining uncorrected sixty (60) days after written notice thereof."""]
            br  []
            p   [] [ str """LICENSEE shall have the right, at any time, to terminate this Agreement without cause by written notice to LICENSOR specifying the date of termination."""]
            br  []
            p   [] [ str """Upon termination, LICENSEE shall destroy all full and partial copies of the Licensed Software."""]
            br  []
            Heading.h6 [] [str "12. Governing Law"] 
            p   [] [ str """This Agreement shall be construed in accordance with the laws of Germany."""]
            br  []
            Heading.h6 [] [str "13. General"]
            p   [] [ str """The parties agree that this Agreement is the complete and exclusive agreement among the parties and supersedes all proposals and prior agreements whether written or oral, and all other communications among the parties relating to the subject matter of this Agreement. This Agreement cannot be modified except in writing and signed by both parties. Failure by either party at any time to enforce any of the provisions of this Agreement shall not constitute a waiver by such party of such provision nor in any way affect the validity of this Agreement.]"""]
            br  []
            p   [] [ str """The invalidity of singular provisions does not affect the validity of the entire understanding. The parties are obligated, however, to replace the invalid provisions by a regulation which comes closest to the economic intent of the invalid provision. The same shall apply mutatis mutandis in case of a gap."""]
            br  []
            p   [] [ str """IN WITNESS WHEREOF, the LICENSEE hereto have caused this Agreement to be duly executed on the date of accepting the license conditions by pressing the Acceptance button.""" ]
        ]
        Modal.close [Modal.Close.OnClick (fun _ -> ShowEulaModal false |> dispatch)] [str "close"]
    ]

let createDropdown dropdownBtnText id children =
    Dropdown.dropdown [] [
        div [Class "dropdown-trigger"] [
            Button.button [Button.Props [AriaHasPopup true; AriaControls id]] [
                span [] [str dropdownBtnText]
                Icon.icon [] [Fa.i [Fa.Solid.AngleDown] []]
            ]
        ]
        Dropdown.menu [Props [Id id ; Role "menu"]] [
            Dropdown.content [] [
                yield! children
            ]   
        ]
    ]


let getDisplayHelpText (model:Model) (dispatch:Msg->unit) =
    
    match model.InformationSectionDisplay with
    |NoHelp         -> []
    |TechnicalScientificDetails ->
        [
            block [ Heading.h4 [] [str "Scientific Details - our research about iMTS-L prediction:" ; Icon.icon [Icon.Props [OnClick (fun _ -> ChangeHelpDisplay NoHelp |> dispatch); Style [Color "red"; Float FloatOptions.Right; Cursor (box "pointer")]]] [Fa.i [Fa.Solid.Times] []]]]
            block [
                ul [] [
                    li [] [str "Backes, S. et al. (2018) Tom70 enhances mitochondrial preprotein import efficiency by binding to internal targeting sequences. J. Cell Biol., 2018: 10.1083/jcb.201708044."]
                    li [] [str "Boos, F. et al. (2018) Detection of Internal Matrix Targeting Signal-like Sequences (iMTS-Ls) in Mitochondrial Precursor Proteins Using the TargetP Prediction Tool. BIO-PROTOCOL, 8, 2018: 10.21769/BioProtoc.2474."]
                ]
            ]
            Heading.h4 [] [str "Technical Details   "]
            block [ str "iMLP is based on the long short-term memory (LSTM) recurrent neural network architecture. These architectures are specially designed for feature detection in sequences and therefore well suited for the recognition of iMTS-Ls. The network is built and consumed using"; a [Href "https://docs.microsoft.com/en-us/cognitive-toolkit/"] [str " CNTK."]]
        ]
    |Contact        ->
        [
            block [ Heading.h4 [] [str "Contact   "; Icon.icon [Icon.Props [OnClick (fun _ -> ChangeHelpDisplay NoHelp |> dispatch); Style [Color "red"; Float FloatOptions.Right; Cursor (box "pointer")]]] [Fa.i [Fa.Solid.Times] []]]]
            block [
                ul [] [
                    li [] [a[Props.Href "mailto:muehlhaus@bio.uni-kl.de"] [str "Timo Mühlhaus"] ; str ", Computational Systems Biology Kaiserslautern"]
                    li [] [a[Props.Href "mailto:schneike@rhrk.uni-kl.de"] [str "Kevin Schneider"] ; str ", Computational Systems Biology Kaiserslautern"]
                ]
            ]
        ]
    |HowToUse       ->
        [
            block [ Heading.h4 [] [str "How To Use   "; Icon.icon [Icon.Props [OnClick (fun _ -> ChangeHelpDisplay NoHelp |> dispatch); Style [Color "red"; Float FloatOptions.Right; Cursor (box "pointer")]]] [Fa.i [Fa.Solid.Times] []]]]
            ul [] [
                li [Class "block"] [
                    block [Heading.h6 [Heading.IsSubtitle] [str "General"]]
                    block [str "The general workflow supported by this website contains these steps:"]
                    block [
                        ol [] [
                            li [Class "block"] [str "Provide input peptide sequences either as text or as fasta file"]
                            li [Class "block"] [str "Select the model closest to your organism of interest (plant or non-plant). Please note that the plant model is highly experimental."]
                            li [Class "block"] [str "Run the iMTS-L propensity profile prediction"] 
                            li [Class "block"] [str "Results are provided on the webpage and can be downloaded"] 
                        ]
                    ]
                ]
                block [
                    li [Class "block"] [
                        block [ Heading.h6 [Heading.IsSubtitle] [str "Input"]]
                        block [ a [OnClick (fun _ -> ChangeHelpDisplay (InputFormat) |> dispatch)] [str "Learn more about the input format here"]]
                        block [
                            strong [] [str "for batch computations with more than 1000 sequences, please "]
                            a [OnClick (fun _ -> ChangeHelpDisplay (Contact) |> dispatch)] [str "contact us"]
                            strong [] [str " or use the "]
                            a [Href "https://github.com/CSBiology/iMLP/releases/"] [str "standalone tool"]
                        ]
                        block [ str "Provide input either via entering a single protein sequence in the textbox or by oploading a file pressing the file link below the textbox."]
                        block [ str "When provided a single sequence via the textbox, a single iMTS-L prediction report will pop in the Result section once the prediction is finished."]
                        block [ str "When provided a file with multiple sequences, iMTS-L prediction results will be generated one after another. You can view the results in the Result section as they are generated, meaning the amount of tabs in the Result section will increase over time while the predictions are finished. You can observe the progress on a per-sequence basis via the progress bar."]
                    ]
                ]
                block [
                    li [Class "block"] [
                        Heading.h6 [Heading.IsSubtitle] [
                            str "Output - Plots"
                        ]
                        block [str "Output of the webpage is twofold: "]
                        block [
                            ol [] [
                                li [Class "block"] [
                                    block [ str "static iMTS-L propensity score heatmap over the protein sequence"]
                                ]
                                li [Class "block"] [
                                    block [str "Interactive iMTS-L propensity score profile area plot"]
                                    block [str "iMTS-L propensity scores above zero indicate iMTS-L stretches. In yeast current studies suggests a length of 12 – 20 amino acids that might vary between species, but further studies are needed to provide solid statistics about the stretch length."]
                                    block [str "These generated plots are fully interactive, meaning you can zoom, pinch, etc."]
                                    block [str "If you like these plots, you can download them by hovering over them and selecting the \"Download plot\" button (the camera image) "]
                                ]
                            ]
                        ]
                    ]
                ]
                block [
                    li [Class "block"] [
                        block [Heading.h6 [Heading.IsSubtitle] [str "Output - Download tab separated results"]]
                        block [str "A download link for your results in tab separated form can be generated using the button on the bottom of the results section."]
                        block [str "Format:"]
                        block [pre [] [str "\"Header\"\t\"Sequence\"\t\"iMTS-L_Propensity_Scores\"\"\n\"FirstHeader\"\t\"A  ...  K\"\t\"0.425000; ... ; 0.056000\"\n...     \t...     \t...     \n\"LastHeader\"\t\"M  ...  F\"\t\"1.905452; ... ; -2.100000\""]]
                        block [str "Once you generated the link, press on the download button to start the download."]
                    ]
                ]
            ]
        ]
    |InputFormat    ->
        [
            br []
            Heading.h4 [] [str "Input format help   "; Icon.icon [Icon.Props [OnClick (fun _ -> ChangeHelpDisplay NoHelp |> dispatch); Style [Color "red"; Float FloatOptions.Right; Cursor (box "pointer")]]] [Fa.i [Fa.Solid.Times] []]]
            br []
            str "The input for both single sequence or file mode has to be in fasta conform format. As this prediction algorithm predicts iMTS-L propensity of proteins, only protein sequences will produce valid output."
            br []
            str "Fasta conform means:"
            ul [] [
                li [] [str "each protein sequence is headed by a single line identifying header, started by the '>' character. In the case of a single sequence input the header can be omitted."]
                li [] [str "The sequence starts in the next line and only consist of valid amino acid characters (ACDEFGHIKLMNPQRSTUVWY)"]
                li [] [str "Ambiguity characters (XZB) are okay"]
                li [] [str "Gap and terminator characters (- and *), as well as O(Pyrrolysine) and J(XLE - XleLeucine or Isoleucine) are filtered out by us. Just keep this in mind when you look at your profiles."]
                li [] [str "All other characters not mentioned above can lead to invalid output."]
            ]
        ]
let displayHelpModal (model:Model) (dispatch:Msg->unit) =
    Modal.modal [Modal.IsActive (not (model.InformationSectionDisplay = NoHelp))] [
        Modal.background [] []
        Modal.content [Props[Style[Width "80%"; BackgroundColor "white"; Padding "1em 1em 1em 1em"; TextAlign TextAlignOptions.Justify]]] [
            Container.container [] [
                Content.content [] (getDisplayHelpText model dispatch)
            ]
        ]
        Modal.close [Modal.Close.OnClick (fun _ -> ChangeHelpDisplay NoHelp |> dispatch)] [str "close"]
    ]


let downloadBtn (location:string) (model:Model) (dispatch: Msg -> unit)=
    Columns.columns[] [
        Column.column [Column.Width (Screen.Desktop, Column.Is2)] [
            str "Enter filename:"
        ]
        Column.column [Column.Width (Screen.Desktop, Column.Is8)] [
            Input.text [
                        Input.Props []
                        Input.Placeholder model.DownloadFileName
                        Input.OnChange (fun e ->    let dname = !!e.target?value
                                                    DownloadFileNameChange dname |> dispatch)
                        ] 
        ]
        Column.column [Column.Width (Screen.Desktop, Column.Is2)] [
            a[
                Props.Download (  if ( model.DownloadFileName.EndsWith(".txt")) then
                                        model.DownloadFileName
                                    else 
                                        sprintf "%s.txt" model.DownloadFileName
                                        )
                Props.Href location
                Props.Class "is-primary is-full-width"
                ] [
                Icon.icon [] [Fa.i [Fa.Solid.Download] []]
                str "Click to download"
            ]
        ]
    ]

let downloadView (model:Model) (dispatch: Msg -> unit) =
    if model.DownloadReady then
        downloadBtn (sprintf "./CsvResults/%s.txt" (model.SessionGuid.ToString())) model dispatch
    else
        Button.button [
                    Button.IsLoading model.HasJobRunning
                    Button.Props [Props.Id "prepareDownload"]
                    Button.CustomClass "is-success" 
                    Button.IsFullWidth
                    Button.OnClick (fun _ -> PrepareDownloadCSV |> dispatch)] [
            str "Prepare Results as tab separated file for download"
        ]


let modeSelection (model : Model) (dispatch : Msg -> unit) =
    match model.SeqMode with
    | Single | NotSelected ->
        Textarea.textarea [
            Textarea.Size Size.IsMedium
            
            Textarea.Placeholder "insert a single amino acid sequence in FASTA format (with or without header)"
            Textarea.OnChange (fun e ->
                let sequence = !!e.target?value
                SingleSequenceInput sequence |> dispatch)
            ] []
    | _ ->
        File.file [File.IsBoxed;File.IsFullWidth;File.HasName] [
            File.label [] [
                singleFileInput [
                    Props.Hidden true
                    OnTextReceived(fun x -> FastaUploadInput (x.Data,x.Name) |> dispatch)
                    ] 
                File.cta [Props [Class "file-cta fastaFileUploadBtn"]] [
                    Heading.h4 [] [str "Click to choose a file"]
                    Icon.icon [] [Fa.i [Fa.Solid.Upload] []]
                ]
                File.name [] [str model.FastaFileInputName]
            ]
        ]


let inputSelection (model : Model) (dispatch : Msg -> unit) =
    let isValidState,buttonMsg = Model.validateInputState model

    let leftHeader,leftAlternative =
        match model.SeqMode with 
        | Single | NotSelected -> "Or upload a ", "file"
        | _ -> "Or insert a single amino acid ", "sequence"

    
    div [] [
        Columns.columns [Columns.CustomClass "ProcessDecision"] [
            Column.column [Column.Width (Screen.Desktop, Column.Is7);Column.CustomClass "leftSelector"] [
                Columns.columns [] [
                    Column.column [Column.Width (Screen.Desktop, Column.Is3)] []
                    Column.column [Column.Width (Screen.Desktop, Column.Is9)] [
                        yield br []
                        yield Heading.h3 [] [str "Input"]
                        yield hr []
                        if (not model.HasValidFasta) then
                            yield p [Class "is-danger"] [str "Your fasta contained invalid characters:"]
                            yield p [Class "is-danger"] [str (sprintf "%A" model.InvalidFastaChars)   ]
                            yield Button.button [Button.CustomClass "is-danger";Button.OnClick (fun _ -> Reset |> dispatch)] [str "Click to reset Input"]
                        yield modeSelection model dispatch
                        yield br []
                        yield
                            Heading.h5 [Heading.IsSubtitle]
                                [
                                    str leftHeader
                                    a [ Class "leftAlternative"
                                        Props.OnClick
                                            (fun _ ->
                                                match model.SeqMode with 
                                                | Single | NotSelected -> SeqModeSelection File |> dispatch
                                                | _ -> SeqModeSelection Single |> dispatch
                                            )] [
                                        str leftAlternative
                                    ]
                                ]
                        yield br []
                    ]
                ]
            ]
            Column.column [Column.Width (Screen.Desktop, Column.Is5);Column.CustomClass "rightSelector"] [
                Columns.columns [] [
                    Column.column [Column.Width (Screen.Desktop, Column.Is8)] [
                        br []
                        Heading.h3 [] [str "Start Prediction"]
                        hr []
                        
                        Button.button [
                            (if isValidState then
                                Button.Disabled false 
                            else 
                                Button.Disabled true)

                            (if isValidState then
                                Button.CustomClass "is-success"
                            else 
                                Button.CustomClass "is-danger" )

                            Button.IsLoading model.HasJobRunning 
                            Button.IsFullWidth
                            Button.CustomClass "startBtn"
                            Button.OnClick (fun e ->
                                match model.SeqMode with
                                | Single    -> SingleSequenceRequest model.SelectedComputationMode |> dispatch
                                | File      -> FastaUploadRequest model.SelectedComputationMode |> dispatch
                                | _ -> ())
                        ] [str buttonMsg ]
                        br []
                        Label.label [Label.Size IsMedium; Label.Props [Style[CSSProp.Color "rgb(237, 125, 49)"]]] [str "select iMLP Model:"]
                        Field.div [Field.IsGrouped] [
                            let isNonPlant = model.SelectedOrganismModel = OrganismModel.NonPlant
                            Control.div [Control.Props [Style[CSSProp.Color "rgb(237, 125, 49)"]]] [

                                Checkbox.checkbox [Props [Style[CSSProp.Color "rgb(237, 125, 49)"] ]] [
                                    Checkbox.input [Props [OnClick (fun _ -> OrganismModelSelection OrganismModel.NonPlant |> dispatch); Checked isNonPlant]]
                                    b [] [ str "NonPlant"]
                                ]
                            ]
                            Control.div [Control.Props [Style[CSSProp.Color "rgb(237, 125, 49)"]]] [
                                Checkbox.checkbox [Props [Style[CSSProp.Color "rgb(237, 125, 49)"]]] [
                                    Checkbox.input [Props[OnClick (fun _ -> OrganismModelSelection OrganismModel.Plant |> dispatch); Checked (not isNonPlant)]]
                                    b [] [ str "Plant"]
                                ]
                            ]
                        ]
                        //Control.div [Control.Props [Style[CSSProp.Color "rgb(237, 125, 49)"]]] [
                        //    Checkbox.checkbox [Props [Style[CSSProp.Color "rgb(237, 125, 49)"]]] [
                        //        Checkbox.input [Props[OnClick (fun _ -> EULAAcceptedChange |> dispatch)]]
                        //        b [] [ str" Use legacy computation model"]
                        //    ]
                        //    div [Class "block"] [
                        //        str "in order to use the targetP-based legacy model you have to agree to iMLP's "
                        //        a [ OnClick (fun _ -> ShowEulaModal true |> dispatch)
                        //            Style [Color "white";]] [
                        //            str "end user license agreement (EULA)"
                        //    ]
                        //    ]
                        //]
                    ]
                    Column.column [Column.Width (Screen.Desktop, Column.Is4)] []
                ]
                
            ]
        ]
    ]


let hero (model : Model) (dispatch : Msg -> unit) =
    Hero.hero [Hero.IsMedium; Hero.CustomClass "csbHero"] [
        Hero.body [] [
            Container.container [] [
                Heading.h1 [Heading.IsSpaced] [
                    str "iMLP : iMTS-L predictor service"
                ]
                br []
                Heading.h4 [Heading.IsSubtitle;Heading.IsSpaced] [
                    str "Additional to their N-terminal matrix-targeting signals (MTSs), many preproteins contain additional internal MTS-like signals (iMTS-Ls) in their mature region which improve the import competence of preproteins and increases the efficiency of their translocation into the mitochondrial matrix."
                    br []
                    br []
                    str "This tool allows the prediction of iMTS-Ls for proteins of interest."
                    br []
                    br []
                    str"For more scientific background, take a look at the "
                    a [
                        OnClick (fun _ -> ChangeHelpDisplay TechnicalScientificDetails |> dispatch)
                        Style [
                            TextDecoration "none"
                            Color "white"
                        ]
                        ][
                            str "'Details'"
                    ]
                    str " section." 
                    ]
            ]
        ]
    ]

let errorModal (showStackTrace:bool) (model : Model) (dispatch : Msg -> unit) =
    let msg,stackTrace =
        match model.ErrorState with
        | Some ex  -> match ex with
                      | :? Fable.Remoting.Client.ProxyRequestException as fe -> fe.Message, fe.ResponseText
                      | _ -> ex.Message,ex.StackTrace
        | None      -> "Unexpected Error","App failed without Exception message"

    Modal.modal [Modal.IsActive model.HasError] [
        Modal.background [] []
        Modal.content [Props[Style[Width "80%"; BackgroundColor "salmon"; Padding "1em 1em 1em 1em"; TextAlign TextAlignOptions.Justify]]] [
            Container.container [] [
                Content.content [] [
                    Heading.h2 [] [str "An error occured. Click the button below to reset the app state:"]
                    div [Class "block"] [ Heading.h5 [] [str msg]]
                    Button.button [Button.CustomClass "is-info resetBtn";Button.OnClick (fun _ -> Reset |> dispatch)] [str "RESET APP STATE"]
                    br []
                    br []
                    Heading.h3 [] [str "If you are a developer and/or interested in the stack trace you can see the error message below."]
                    br []
                    Button.button [Button.Color IsInfo; Button.OnClick (fun _ -> ChangeErrorStateVisibility |> dispatch)] [str "toggle stack trace"]
                    br []
                    if showStackTrace then Content.content [Content.Props [Hidden (not model.ShowErrorStack)]] [
                        div [Class "block"] [ Heading.h5 [] [str "StackTrace:"]]
                        pre [Class "block is-white"] (
                                stackTrace.Split([|" at "|],System.StringSplitOptions.None)
                                |> Array.map (fun blck ->
                                    p [Class "block"] [str blck]
                                )
                        )
                    ]
                ]
            ]
        ]
        Modal.close [Modal.Close.OnClick (fun _ -> Reset |> dispatch)] [str "close"]
    ]

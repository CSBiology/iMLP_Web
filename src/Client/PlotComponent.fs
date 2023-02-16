module PlotComponent

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

open Shared
open JSInterop
open AppModel
open StateHandling

open Feliz
open Feliz.Plotly

let plotIMLPResult (res: IMLPResult) =
    div [Style [MinHeight 600]] [
        Plotly.plot [
            plot.traces [
                traces.scatter [
                    scatter.x [ 1 .. res.Sequence.Length]
                    scatter.y res.PropensityScores
                    scatter.mode.lines
                    scatter.line [
                        line.color "rgb(237, 125, 49, 0.9)";
                        line.width 2.5;
                        line.shape.spline
                    ]
                    scatter.fill.tozeroy
                    scatter.name "Propensity Score"
                ]
            ]
            plot.layout [
                layout.paperBgcolor "rgba(0,0,0,0)"
                layout.plotBgcolor "white"
                layout.font [font.family "Arial"; font.size 18]
                layout.legend [
                    legend.x 0.02
                    legend.y 0.98
                    legend.traceorder.normal
                    legend.bgcolor "rgba(222, 235, 247, 0.6)"
                    legend.bordercolor "rgb(68, 84, 106)"
                    legend.borderwidth 2
                ]
                layout.xaxis [
                    xaxis.title "Score"
                    xaxis.showgrid false
                    xaxis.showline true
                    xaxis.mirror.allticks
                    xaxis.zeroline true
                    xaxis.tickmode.auto
                    xaxis.ticks.inside
                    xaxis.tickfont [tickfont.family "Arial"; tickfont.size 18]
                ]
                layout.yaxis [
                    yaxis.title "Index of AminoAcid"
                    yaxis.showgrid false
                    yaxis.showline true
                    yaxis.mirror.allticks
                    yaxis.zeroline true
                    yaxis.tickmode.auto
                    yaxis.ticks.inside
                    yaxis.tickfont [tickfont.family "Arial"; tickfont.size 18]
                ]
            ]
            plot.config [
                config.responsive true
                config.toImageButtonOptions [toImageButtonOptions.format.svg "iMLP_propensity_plot.svg"]
            ]
        ]
    ]
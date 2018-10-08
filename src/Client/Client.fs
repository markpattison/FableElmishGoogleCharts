module Client

open Elmish
open Elmish.React

open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open TheGamma.Common

open Shared

open Fulma
open Fable.Core

type View = ShowChart | NoChart
type Model = { View: View; Values: Values option; NeedToDrawChart: bool }

type Msg =
    | RequestValues
    | ReceiveValues of Result<Values, exn>
    | PageWithChart
    | PageWithoutChart
    | DrawChart

module Server =

    open Shared
    open Fable.Remoting.Client

    let api : IDemoApi =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<IDemoApi>

let init () : Model * Cmd<Msg> =
    { View = ShowChart; Values = None; NeedToDrawChart = false }, []

let loadDataCmd =
    Cmd.ofAsync
        Server.api.demoValues
        ()
        (Ok >> ReceiveValues)
        (Error >> ReceiveValues)

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | RequestValues -> currentModel, loadDataCmd
    | ReceiveValues (Ok values) -> { currentModel with Values = Some values }, Cmd.ofMsg DrawChart
    | ReceiveValues (Error _) -> currentModel, []
    | DrawChart ->
        match currentModel.Values with
        | Some values ->
            let asyncData1 = async { return values |> Array.mapi (fun i v -> i, (fst v, sprintf "bottom: %.1f" (fst v))) }
            let asyncData2 = async { return values |> Array.mapi (fun i v -> i, (snd v - fst v, sprintf "top: %.1f" (snd v))) }
            let series1 = TheGamma.Series.series<_, _>.create (asyncData1, "keyName", "valueName", "bottom")
            let series2 = TheGamma.Series.series<_, _>.create (asyncData2, "keyName", "valueName", "top")
            let allSeries = TheGamma.Series.series<_, _>.values [| series1; series2 |]
            let chart = TheGamma.GoogleCharts.chart.areas2 allSeries
            let chart1 = chart.set(isStacked = true, series = JsInterop.createObj [ "0" ==> JsInterop.createObj [ "areaOpacity" ==> "0.0"; "visibleInLegend" ==> "false"; "lineWidth" ==> "0" ] ] )
            let chart2 =     chart1.hAxis(title = "horizontal axis")
            let chart3 =     chart2.vAxis(title = "vertical axis")
            chart3.show("Chart1")
        | None -> ()
        { currentModel with NeedToDrawChart = false }, []
    | PageWithChart -> { currentModel with View = ShowChart; NeedToDrawChart = true }, []
    | PageWithoutChart -> { currentModel with View = NoChart }, []

let button txt onClick =
    Button.button
        [ Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let view (model : Model) (dispatch : Msg -> unit) =
    let chartRef element =
        if not (isNull element) && model.NeedToDrawChart then
            dispatch DrawChart
    let content =
        match model.View with
        | NoChart ->
            div []
              [ div [] [ str "Page without chart" ]
                button "Show page with chart" (fun _ -> dispatch PageWithChart) ]
        | ShowChart ->
            div []
              [ div [] [ str "Page with chart" ]
                button "Get data" (fun _ -> dispatch RequestValues)
                button "Show page without chart" (fun _ -> dispatch PageWithoutChart )
                div [ Id "Chart1"; Ref chartRef ] [] ]
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "SAFE Template" ] ] ]
          content ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
|> Program.run

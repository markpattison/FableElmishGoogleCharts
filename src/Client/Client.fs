module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Shared

open Fulma

type Model = { Values: Values option }

type Msg =
| RequestValues
| ReceiveValues of Result<Values, exn>

module Server =

    open Shared
    open Fable.Remoting.Client

    let api : IDemoApi =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<IDemoApi>


let init () : Model * Cmd<Msg> =
    let initialModel = { Values = None }
    initialModel, []

let loadCountCmd =
    Cmd.ofAsync
        Server.api.demoValues
        ()
        (Ok >> ReceiveValues)
        (Error >> ReceiveValues)

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.Values, msg with
    | _, RequestValues ->
        currentModel, loadCountCmd
    | _, ReceiveValues (Ok values)->
        let nextModel = { Values = Some values }
        let series = TheGamma.Series.series<_, _>.values values
        let chart = TheGamma.GoogleCharts.chart.line series
        TheGamma.GoogleCharts.chart.show chart "Chart1"
        nextModel, []
    | _ -> currentModel, []

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let show model =
    match model.Values with
    | Some values -> System.String.Join(",", values)
    | None -> "Not loaded"

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "SAFE Template" ] ] ]

          Container.container []
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str ("Values: " + show model) ] ]
                Columns.columns []
                    [ Column.column [] [ button "Get values" (fun _ -> dispatch RequestValues) ] ] ]
          div [ Id "Chart1" ] [] ]

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
#if DEBUG
// |> Program.withDebugger
#endif
|> Program.run

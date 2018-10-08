open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared

open Fable.Remoting.Server
open Fable.Remoting.Giraffe

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us

let getDemoValues() : Task<Values> = task { return [| 1.0, 4.0; 3.0, 5.0; 2.0, 3.0 |] }

let demoApi = {
    demoValues = getDemoValues >> Async.AwaitTask
}

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue demoApi
    |> Remoting.buildHttpHandler

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    use_gzip
}

run app

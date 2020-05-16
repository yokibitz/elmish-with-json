module App

open Browser
open Browser.Types
open Elmish
open Elmish.React
open Feliz
open Fable.SimpleHttp
open Thoth.Json

type Product =
    { name: string
      price: float }

type StoreInfo =
    { name: string
      since: int
      daysOpen: string list
      products: Product list }

type State =
    { StoreInfo: Deferred<Result<StoreInfo, string>> }

type Msg = LoadStoreInfo of AsyncOperationStatus<Result<string, string>>

let init() = { StoreInfo = HasNotStartedYet }, Cmd.ofMsg (LoadStoreInfo Started)

let parseStoreInfo = Decode.Auto.fromString<StoreInfo>

let update msg state =
    match msg with
    | LoadStoreInfo Started ->
        let nextState = { state with StoreInfo = InProgress }

        let loadStoreInfo =
            async {
                do! Async.Sleep 1500
                let! (statusCode, storeInfo) = Http.get "/store.json"
                if statusCode = 200
                then return LoadStoreInfo(Finished(Ok storeInfo))
                else return LoadStoreInfo(Finished(Error "Could not load the store information"))
            }
        nextState, Cmd.fromAsync loadStoreInfo
    | LoadStoreInfo(Finished(Ok storeInfoJson)) ->
        match parseStoreInfo storeInfoJson with
        | Ok storeInfo ->
            let nextState = { state with StoreInfo = Resolved(Ok storeInfo) }
            nextState, Cmd.none
        | Error error ->
            let nextState = { state with StoreInfo = Resolved(Error error) }
            nextState, Cmd.none
    | LoadStoreInfo(Finished(Error httpError)) ->
        let nextState = { state with StoreInfo = Resolved(Error httpError) }
        nextState, Cmd.none

let render state dispatch =
    match state.StoreInfo with
    | HasNotStartedYet -> Html.none
    | InProgress -> Html.h1 "Loading..."
    | Resolved(Error errorMsg) ->
        Html.h1
            [ prop.style [ style.color.red ]
              prop.text errorMsg ]
    | Resolved(Ok storeInfo) ->
        Html.div
            [ Html.h1 storeInfo.name
              Html.ul [ for product in storeInfo.products -> Html.li product.name ] ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run

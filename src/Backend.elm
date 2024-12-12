module Backend exposing (app, init)

import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import Pages exposing (ServerRequest(..))
import Set exposing (Set)
import Task
import Types exposing (..)


app =
    Pages.backend sendToFrontend
        { init = init
        , onRequest =
            \(ServerRequest url) model ->
                Task.succeed
                    ( model
                    , BlogPostData "TODO get real data"
                    , []
                    )
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( MyBackendModel, Cmd MyBackendMsg )
init =
    ( { counter = 0 }, Cmd.none )


update : MyBackendMsg -> MyBackendModel -> ( MyBackendModel, Cmd MyBackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            ( model
            , sendToFrontend clientId <|
                Pages.toFrontendMsg
                    (CounterNewValue model.counter clientId)
            )

        Noop ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> MyToBackend -> MyBackendModel -> ( MyBackendModel, Cmd MyBackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        CounterIncremented ->
            let
                newCounter =
                    model.counter + 1
            in
            ( { model | counter = newCounter }, broadcast (Pages.toFrontendMsg (CounterNewValue newCounter clientId)) )

        CounterDecremented ->
            let
                newCounter =
                    model.counter - 1
            in
            ( { model | counter = newCounter }, broadcast (Pages.toFrontendMsg (CounterNewValue newCounter clientId)) )


subscriptions : MyBackendModel -> Sub MyBackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        ]

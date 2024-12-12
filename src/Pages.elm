module Pages exposing (BackendMsg, Blockable(..), HeadTag, Model, Msg, ServerRequest(..), ToBackend, ToFrontend, backend, frontend)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Html
import Lamdera exposing (ClientId, SessionId, Url, sendToBackend, sendToFrontend)
import Task exposing (Task)


type Model model
    = Initializing
    | UserModel model


type Msg msg
    = UserMsg msg
    | SendUrlChangeToBackend Url


type Blockable input output
    = Static output
      -- TODO consider adding in `NonBlocking output (input -> msg)`
    | Blocking (input -> output)


{-| can't use it in the prototype until pre-rendered HTML is possible so it's empty for now
-}
type HeadTag
    = HeadTag


type ServerRequest
    = ServerRequest Url


type ToFrontend toFrontend dataError data
    = UserToFrontend toFrontend
    | UrlChangeDataReceived (Result dataError data)


type ToBackend toBackend
    = UserToBackend toBackend
    | UrlChanged Url


type BackendMsg dataError data backendModel backendMsg
    = UserBackendMsg backendMsg
    | Thing ClientId (Result dataError ( backendModel, data, List HeadTag ))


frontend :
    { init : Url -> Key -> Blockable (Result dataError data) ( model, Cmd frontendMsg )
    , view : model -> Browser.Document frontendMsg
    , update : frontendMsg -> model -> ( model, Cmd frontendMsg )
    , updateFromBackend : toFrontend -> model -> ( model, Cmd frontendMsg )
    , subscriptions : model -> Sub frontendMsg
    , onUrlRequest : UrlRequest -> frontendMsg
    , onUrlChange : Url -> Blockable (Result dataError data) frontendMsg
    }
    ->
        { init : Url -> Browser.Navigation.Key -> ( Model model, Cmd (Msg frontendMsg) )
        , view : Model model -> Browser.Document (Msg frontendMsg)
        , update : Msg frontendMsg -> Model model -> ( Model model, Cmd (Msg frontendMsg) )
        , updateFromBackend : ToFrontend toFrontend dataError data -> Model model -> ( Model model, Cmd (Msg frontendMsg) )
        , subscriptions : Model model -> Sub (Msg frontendMsg)
        , onUrlRequest : Browser.UrlRequest -> Msg frontendMsg
        , onUrlChange : Url -> Msg frontendMsg
        }
frontend config =
    { init =
        \url key ->
            case config.init url key of
                Static ( model, cmd ) ->
                    ( UserModel model, cmd |> Cmd.map UserMsg )

                Blocking blockingInitFn ->
                    -- TODO use blockingInitFn
                    ( Initializing
                    , sendToBackend (UrlChanged url)
                    )
    , view =
        \model ->
            case model of
                Initializing ->
                    Browser.Document "Initializing" []

                UserModel userModel ->
                    config.view userModel
                        |> mapDocument UserMsg
    , update =
        \msg model ->
            case msg of
                UserMsg userMsg ->
                    case model of
                        Initializing ->
                            ( Initializing, Cmd.none )

                        UserModel userModel ->
                            let
                                ( newUserModel, cmd ) =
                                    config.update userMsg userModel
                            in
                            ( UserModel newUserModel, cmd |> Cmd.map UserMsg )

                SendUrlChangeToBackend serverRequest ->
                    ( model, sendToBackend (UrlChanged serverRequest) )
    , updateFromBackend =
        \toFrontend model ->
            case toFrontend of
                UserToFrontend userToFrontend ->
                    case model of
                        Initializing ->
                            ( Initializing, Cmd.none )

                        UserModel userModel ->
                            let
                                ( newUserModel, cmd ) =
                                    config.updateFromBackend userToFrontend userModel
                            in
                            ( UserModel newUserModel, cmd |> Cmd.map UserMsg )

                UrlChangeDataReceived result ->
                    -- TODO use result
                    ( model, Cmd.none )
    , subscriptions =
        \model ->
            case model of
                Initializing ->
                    Sub.none

                UserModel userModel ->
                    config.subscriptions userModel
                        |> Sub.map UserMsg
    , onUrlRequest = \urlRequest -> urlRequest |> config.onUrlRequest |> UserMsg
    , onUrlChange =
        \url ->
            case config.onUrlChange url of
                Static msg ->
                    UserMsg msg

                Blocking blockingOnUrlChange ->
                    -- TODO use blockingOnUrlChange
                    SendUrlChangeToBackend url
    }


backend :
    { init : ( backendModel, Cmd backendMsg )
    , onRequest : ServerRequest -> backendModel -> Task dataError ( backendModel, data, List HeadTag )
    , update : backendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
    , updateFromFrontend : SessionId -> ClientId -> toBackend -> backendModel -> ( backendModel, Cmd backendMsg )
    , subscriptions : backendModel -> Sub backendMsg
    }
    ->
        { init : ( backendModel, Cmd (BackendMsg dataError data backendModel backendMsg) )
        , update : BackendMsg dataError data backendModel backendMsg -> backendModel -> ( backendModel, Cmd (BackendMsg dataError data backendModel backendMsg) )
        , updateFromFrontend : SessionId -> ClientId -> ToBackend toBackend -> backendModel -> ( backendModel, Cmd (BackendMsg dataError data backendModel backendMsg) )
        , subscriptions : backendModel -> Sub (BackendMsg dataError data backendModel backendMsg)
        }
backend config =
    { init =
        config.init
            |> Tuple.mapSecond (Cmd.map UserBackendMsg)
    , update =
        \msg model ->
            case msg of
                UserBackendMsg userBackendMsg ->
                    config.update userBackendMsg model
                        |> Tuple.mapSecond (Cmd.map UserBackendMsg)

                Thing clientId onRequestDataResult ->
                    case onRequestDataResult of
                        -- TODO use headTags for pre-rendering HTML eventually
                        Ok ( newModel, data, headTags ) ->
                            ( newModel, sendToFrontend clientId (UrlChangeDataReceived (Ok data)) )

                        Err errorData ->
                            ( model, sendToFrontend clientId (UrlChangeDataReceived (Err errorData)) )
    , updateFromFrontend =
        \sessionId clientId toBackend backendModel ->
            case toBackend of
                UrlChanged url ->
                    ( backendModel
                    , config.onRequest (ServerRequest url) backendModel
                        |> Task.attempt (Thing clientId)
                    )

                UserToBackend userToBackend ->
                    config.updateFromFrontend sessionId clientId userToBackend backendModel
                        |> Tuple.mapSecond (Cmd.map UserBackendMsg)
    , subscriptions = \model -> config.subscriptions model |> Sub.map UserBackendMsg
    }


mapDocument : (a -> b) -> Browser.Document a -> Browser.Document b
mapDocument mapFn document =
    { title = document.title
    , body = List.map (Html.map mapFn) document.body
    }

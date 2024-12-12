module Frontend exposing (Model, app)

import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Lamdera exposing (sendToBackend)
import Pages exposing (Blockable(..))
import Types exposing (..)


type alias Model =
    MyFrontendModel


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.frontend is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Pages.frontend sendToBackend
        { init = \_ _ -> init
        , update = update
        , updateFromBackend = updateFromBackend
        , view =
            \model ->
                { title = "v1"
                , body = [ view model ]
                }
        , subscriptions = \_ -> Sub.none
        , onUrlChange = \_ -> Static FNoop
        , onUrlRequest = \_ -> FNoop
        }


init : Blockable (Result dataError data) ( Model, Cmd MyFrontendMsg )
init =
    Static ( { counter = 0, clientId = "" }, Cmd.none )


update : MyFrontendMsg -> Model -> ( Model, Cmd MyFrontendMsg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }
              -- TODO
            , --Cmd.none
              sendToBackend (Pages.toBackendMsg CounterIncremented)
            )

        Decrement ->
            ( { model | counter = model.counter - 1 }
            , Cmd.none
              --TODO
              -- sendToBackend (Pages.toBackendMsg CounterDecremented)
            )

        FNoop ->
            ( model, Cmd.none )


updateFromBackend : MyToFrontend -> Model -> ( Model, Cmd MyFrontendMsg )
updateFromBackend msg model =
    case msg of
        CounterNewValue newValue clientId ->
            ( { model | counter = newValue, clientId = clientId }, Cmd.none )


view : Model -> Html MyFrontendMsg
view model =
    Html.div [ style "padding" "30px" ]
        [ Html.button [ onClick Increment ] [ text "+" ]
        , Html.text (String.fromInt model.counter)
        , Html.button [ onClick Decrement ] [ text "-" ]
        , Html.div [] [ Html.text "Click me then refresh me!" ]
        ]

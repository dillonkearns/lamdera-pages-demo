module Evergreen.V1.Types exposing (..)

import Lamdera


type alias FrontendModel =
    { counter : Int
    , clientId : String
    }


type alias BackendModel =
    { counter : Int
    }


type FrontendMsg
    = Increment
    | Decrement
    | FNoop


type ToBackend
    = CounterIncremented
    | CounterDecremented


type BackendMsg
    = ClientConnected Lamdera.SessionId Lamdera.ClientId
    | Noop


type ToFrontend
    = CounterNewValue Int String

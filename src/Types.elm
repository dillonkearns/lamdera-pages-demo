module Types exposing (..)

import Lamdera exposing (ClientId, SessionId)
import Pages
import Set exposing (Set)


type alias BackendModel =
    Pages.Model MyBackendModel


type alias FrontendModel =
    Pages.Model MyFrontendModel


type alias FrontendMsg =
    Pages.FrontendMsg MyFrontendMsg


type alias ToBackend =
    Pages.ToBackend MyToBackend


type alias BackendMsg =
    Pages.BackendMsg MyDataError MyData MyBackendModel MyBackendMsg


type alias ToFrontend =
    Pages.ToFrontend MyToFrontend MyDataError MyData


type MyDataError
    = Unexpected String


type MyData
    = BlogPostData String


type alias MyBackendModel =
    { counter : Int
    }


type alias MyFrontendModel =
    { counter : Int
    , clientId : String
    }


type MyFrontendMsg
    = Increment
    | Decrement
    | FNoop


type MyToBackend
    = CounterIncremented
    | CounterDecremented


type MyBackendMsg
    = ClientConnected SessionId ClientId
    | Noop


type MyToFrontend
    = CounterNewValue Int String

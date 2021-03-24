module Main exposing (..)

import Browser exposing (Document)
import Html exposing (div, textarea, label, text)
import Html.Attributes exposing (class, id, for, value)
import Html.Events exposing (onInput)
import H.Render exposing (renderElm)

main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

init : flags -> (Model, Cmd Msg)
init _ =
    ( { htmlText = ""
      , elmText = ""
      }
    , Cmd.none )

type alias Model =
    { htmlText : String
    , elmText : String
    }

type Msg
    = InsertHtmlText String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InsertHtmlText html -> 
            (
                { model |
                      htmlText = html 
                    , elmText =
                        case renderElm html of
                            Ok txt ->
                                txt
                            Err txt ->
                                txt
                }, Cmd.none)

view : Model -> Document Msg
view model =
    { title = "Dashboard Application"
    , body =
        [ div [class "container"]
            [ label [for "html-text"] [text "Html"]
            , textarea [id "html-text", class "nes-textarea", onInput InsertHtmlText] []
            , label [for "elm-text"] [text "Elm"]
            , textarea [id "elm-text", class "nes-textarea", value model.elmText] []
            ]
        ]
    }


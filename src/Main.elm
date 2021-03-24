module Main exposing (..)

import Browser exposing (Document)
import Html exposing (div, textarea, label, text, i, a, p)
import Html.Attributes exposing (class, id, for, href, target)
import Html.Events exposing (onInput)
import H.Render exposing (render2Elm)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { htmlText : String
    , elmText : String
    , repo : String
    }

type Msg
    = InsertHtmlText String


init : flags -> (Model, Cmd Msg)
init _ =
    ( { htmlText = ""
      , elmText = ""
      , repo = "https://github.com/dev-danilosilva/html-to-elm"
      }
    , Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InsertHtmlText html -> 
            (
                { model |
                      htmlText = html 
                    , elmText =
                        case render2Elm html of
                            Ok txt ->
                                txt
                            Err txt ->
                                txt
                }, Cmd.none)

view : Model -> Document Msg
view model =
    { title = "Html 2 Elm!"
    , body =
        [ div [class "container"]
            [ label [for "html-text"] [text "Html"]
            , textarea [id "html-text", class "nes-textarea", onInput InsertHtmlText] []
            , div [class "nes-container is-dark with-title"]
                [ p [class "title"] [text "Elm"]
                , p [] [text model.elmText]
                ]
            , a [href model.repo, target "blank_"]
                [ i [class "nes-icon github is-large"] []
                ]
            ]
        ]
    }


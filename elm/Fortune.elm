module Fortune exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task

main =
    App.program
      { init = init "Reading your fortune..."
      , view = view
      , update = update
      , subscriptions = subscriptions
      }

type alias Model = { fortune : String }

init : String -> (Model, Cmd Msg)
init s = (Model s, getRandomFortune)

type Msg
  = FetchSuccess String
  | FetchFail Http.Error
  | NewFortune

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FetchSuccess f ->
            (Model f, Cmd.none)
        FetchFail _ ->
            ( Model "Sorry, I failed in reading your fortune! Try again?"
            , Cmd.none )
        NewFortune ->
            (model, getRandomFortune)

view : Model -> Html Msg
view model =
    div []
      [ h2 [] [text model.fortune]
      , button [ onClick NewFortune ] [ text "New Fortune" ]
      ]

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

serverURL = "http://127.0.0.1:1234/"
getRandomFortune : Cmd Msg
getRandomFortune =
    let url = serverURL ++ "/fortune"
    in Task.perform FetchFail FetchSuccess (Http.get Json.string url)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)

import Json.Decode as Json
import String

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }

-- model
type alias Model =
  { email : String
  , hosting : Int
  , period :  Int
  , days : Int
  , total : Int
  }

init : (Model, Cmd Msg)
init =
  (Model "" 105 0 0 0, Cmd.none)

-- update
type Msg
  = Submit
  | HostingChanged Int
  | PeriodChanged Int
  | DaysChanged Int

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Submit ->
      ({model | total = 10}, Cmd.none)
    HostingChanged hosting ->
        ({ model | hosting = hosting }, Cmd.none)
    PeriodChanged period ->
      ({ model | period = period }, Cmd.none)
    DaysChanged days ->
      ({ model | days = days }, Cmd.none)

-- SUBSCRIPTIONS
calculateCost : Model -> Int
calculateCost m =
  (m.period * m.hosting) + (m.days * 691 * m.period)

-- view
hostOption host =
  option [ value (toString host.cost) ] [ text host.name ]

durationOption duration =
  option [value (toString duration) ] [ text (toString duration)]

targetValueIntDecoder : Json.Decoder Int
targetValueIntDecoder =
  targetValue `Json.andThen` \val ->
    case String.toInt val of
      Ok i -> Json.succeed i
      Err err -> Json.fail err

view : Model -> Html Msg
view model =
  Html.form [] [
    h2 [] [
      text "Estimez votre projet"
    ],
    div [] [
      label[ for "email" ][ text "Email: " ],
      input [ id "email", placeholder "my@email.com" ] []
    ],
    div [] [
      label [ for "hosting" ][ text "Hebergement: " ],
      select [ id "hosting", on "change" (Json.map HostingChanged targetValueIntDecoder)]
      (List.map hostOption [{name="Saas", cost=105}, {name="Dédié", cost=536}, {name="Critique", cost=1072}])
    ],
    div [] [
      label [ for "months" ][ text "Pendant: " ],
      select [ id "months", on "change" (Json.map PeriodChanged targetValueIntDecoder) ]
      (List.map durationOption [0..12]), -- options
      span [][ text " mois"]
    ],
    div [] [
      label [ for "help" ][ text "Je pense avoir besoin d'être accompagné: " ],
      select [ id "help", on "change" (Json.map DaysChanged targetValueIntDecoder) ]
      (List.map durationOption [0..31]), -- options
      span [][ text " jours par mois"]
    ],
    div [] [
      Html.span [][ text ("Total: " ++ (toString <| calculateCost model) ++ " euros (€)/mois")]
    ],
    button [ onClick Submit ] [text "Send to Anybox"]
  ]

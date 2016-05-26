port module Estimation exposing (..)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue, onCheck)

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
type alias Model = {
  hosting : Int,
  period : Int,
  days : Int,
  email : String,
  toCall: Bool,
  total : Int
}

init : (Model, Cmd Msg)
init = (Model 105 0 0 "" False 0, Cmd.none)

-- update
type Msg
  = Submit
  | HostingChanged Int
  | PeriodChanged Int
  | DaysChanged Int
  | NeedACall

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Submit ->
      (model, Cmd.none)
    HostingChanged hosting ->
        ({ model | hosting = hosting }, Cmd.none)
    PeriodChanged period ->
      ({ model | period = period }, Cmd.none)
    DaysChanged days ->
      ({ model | days = days }, Cmd.none)
    NeedACall ->
      ({ model | toCall = not model.toCall }, Cmd.none)

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

neddACallClass: Bool -> String
neddACallClass need =
  if need then
    "show"
  else
    "hidden"

view : Model -> Html Msg
view model =
  div [ class "container" ] [
    div [ class "row" ] [
      div [ class "jumbotron" ] [
        Html.form [] [
          h2 [] [
            text "Estimez votre projet"
          ],
          div [ class "form-inline" ] [
            div [ class "form-group" ] [
              label [ for "hosting" ][ text "Hebergement: " ],
              select [ id "hosting", class "form-control", on "change" (Json.map HostingChanged targetValueIntDecoder)]
                (List.map hostOption [{name="Saas", cost=105}, {name="Dédié", cost=536}, {name="Critique", cost=1072}])
            ],
            div [ class "form-group" ] [
              label [ for "months" ][ text "Pendant: " ],
              select [ id "months", class "form-control", on "change" (Json.map PeriodChanged targetValueIntDecoder) ]
                (List.map durationOption [0..12]), -- options
              span [][ text " mois" ]
            ]
          ],
          div [ class "form-inline" ] [
            div [ class "form-group" ] [
              label [ for "help" ][ text "Je pense avoir besoin d'être accompagné " ],
              select [ id "help", class "form-control", on "change" (Json.map DaysChanged targetValueIntDecoder) ]
                (List.map durationOption [0..31]), -- options
              span [][ text " jours par mois" ]
            ]
          ],
          div [] [
            h2 [ class "text-center" ][ text ("C'est au total " ++ (toString <| calculateCost model) ++ " €/mois maximum !")]
          ],

          div [ class "checkbox" ] [
            label [ for "needACall" ][
              input [ id "needACall", type' "checkbox", checked model.toCall, onCheck (\_ -> NeedACall) ][],
              text "Je souhaite être contacté par l'équipe commerciale:"
            ]
          ],
          div [ class (neddACallClass model.toCall) ] [
            div [ class "form-group" ] [
              label [ for "email" ][ text "Email: " ],
              input [ id "email", class "form-control", placeholder "my@email.com" ] []
            ],
            button [ class "btn btn-info", onClick Submit ] [ text "Send to Anybox" ]
          ]
        ]
      ]
    ]
  ]

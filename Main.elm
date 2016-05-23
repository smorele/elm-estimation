import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)

import Html.Events exposing (onClick)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- model
type alias Host = {
  name : String,
  cost : Int
}
type alias Model =
  { email : String
  , hosting : Host
  , period :  Int
  , interventionDays : Int
  , total : Int
  }

init : (Model, Cmd Msg)
init =
  (Model "" {name="Saas", cost=105} 0 0 0, Cmd.none)

-- update
type Msg = Submit | Calculate

calculate : Model -> Int
calculate model =
  model.period * 10 -- test

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Submit ->
      ({model | total = calculate model}, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- view
hostOption host =
  option [ value (toString host.cost) ] [ text host.name ]

durationOption duration =
  option [value (toString duration) ] [ text (toString duration)]

view : Model -> Html Msg
view model =
  Html.div []
    [ h2 [] [ text "Estimez votre projet"]
    , input [ placeholder "my@email.com" ] []
    , select []
      (List.map hostOption [{name="Saas", cost=105}, {name="Dédié", cost=536}, {name="Critique", cost=1072}])
    , select []
      (List.map durationOption [0..12])
    , select []
      (List.map durationOption [0..31])
    , Html.span [][text (toString model.total)]
    , button [ onClick Submit ] [text "Send to Anybox"]
    ]

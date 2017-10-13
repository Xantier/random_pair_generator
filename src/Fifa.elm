module Fifa exposing (main)

import Html exposing (Html, div, text, button, ul, li)
import Html.Events exposing (onClick)
import List.Extra exposing (getAt, removeAt)
import Random exposing (Seed, generate, initialSeed, int, maxInt, minInt, step)


type alias Model =
    {
    teams : List String,
    players : List String,
    pairs : List (Maybe String, Maybe String)
    }


model : Model
model =
    {
        teams = ["Spain", "Germany", "Brazil", "France", "Belgium", "Portugal", "Italy", "England", "Argentina", "Uruguay", "Netherlands", "Colombia", "Poland", "Mexico", "Chile", "Ireland"],
        players = ["Ian", "Simo", "Enrico", "Jussi", "Gio", "Devoy", "Jason", "Stevie"],
        pairs = []
    }


type Msg
    = ShuffleIt
    | FetchNext
    | Shuffle Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleIt ->
            ( model, generate Shuffle (int minInt maxInt) )

        Shuffle seedInt ->
            ( { model | teams = shuffleList (initialSeed seedInt) model.teams
                      , players = shuffleList (initialSeed seedInt) model.players }
                      , Cmd.none )

        FetchNext ->
            ( { model | pairs = fetchNext model.teams model.players model.pairs
                      , teams = List.drop 1 model.teams
                      , players = List.drop 1 model.players}, Cmd.none)

fetchNext : List String -> List String -> List (Maybe String, Maybe String) -> List (Maybe String, Maybe String)
fetchNext teams players pairs = (List.head teams, List.head players) :: pairs

shuffleList : Seed -> List a -> List a
shuffleList seed list =
    shuffleListHelper seed list []


shuffleListHelper : Seed -> List a -> List a -> List a
shuffleListHelper seed source result =
    if List.isEmpty source then
        result
    else
        let
            indexGenerator =
                int 0 ((List.length source) - 1)

            ( index, nextSeed ) =
                step indexGenerator seed

            valAtIndex =
                getAt index source

            sourceWithoutIndex =
                removeAt index source
        in
            case valAtIndex of
                Just val ->
                    shuffleListHelper nextSeed sourceWithoutIndex (val :: result)

                Nothing ->
                    Debug.crash "generated an index outside list"

extract: Maybe String -> String
extract val =
  case val of
  Just value ->
      value

  Nothing ->
      Debug.crash "Ran out of elements"

extractPairs : (Maybe String, Maybe String) -> String
extractPairs (team, player) = extract player ++ " - " ++ extract team


renderList : List (Maybe String, Maybe String) -> Html msg
renderList lst =
    ul []
        (List.map (\l -> li [] [ text (extractPairs l) ]) lst)


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text <| toString <| model.players ]
        , div [] [ button [ onClick ShuffleIt ] [ text "Shuffle" ] ]
        , div [] [ button [ onClick FetchNext ] [ text "Display next pair" ] ]
        , div [] [ (renderList model.pairs) ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( model, Cmd.none )
        }
module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import List


type alias Story =
    { id : String, narrative : String, options : List Option }


type alias Option =
    { title : String, toId : String }


type alias EntireStory =
    List Story


type alias Save =
    String


type alias Saves =
    List String


type Mode
    = Game
    | SaveScreen


type Msg
    = Choice String
    | Load String
    | OpenSave
    | SaveGame


type alias Model =
    { entireStory : EntireStory, currentStory : String, saves : Saves, mode : Mode }


bigStory : List Story
bigStory =
    [ Story "main"
        "Kenny Kallmann fell into a portal and came out somewhere. What would he do?"
        [ Option "Go left" "end1"
        , Option "Go right" "rightChoice"
        , Option "Just stay there" "stayChoice"
        ]
    , Story "end1"
        "You die. "
        [ Option "Restart" "main" ]
    , Story "rightChoice"
        "Kenny meets Jeremy Chen in an alley behind Beijing Cafe. "
        [ Option "Fight Him!" "fightChoice"
        , Option "Face off Jeremy in a math competition" "mathChoice"
        ]
    ]


divStyle : Attribute msg
divStyle =
    style
        [ ( "display", "flex" )
        , ( "justify-content", "center" )
        , ( "font-size", "1em" )
        , ( "align-items", "center" )
        , ( "flex-direction", "column" )
        ]


getStory : EntireStory -> String -> Story
getStory stories id =
    let
        pred : Story -> Bool
        pred story =
            (story.id == id)
    in
        case List.head (List.filter pred stories) of
            Nothing ->
                Debug.crash "INVALID ID"

            Just story ->
                story


renderGame : Story -> Html Msg
renderGame story =
    let
        renderOption : Option -> Html Msg
        renderOption option =
            button [ onClick (Choice option.toId) ] [ text option.title ]
    in
        div []
            [ h1 [] [ text "Story!" ]
            , h2 [] [ text story.narrative ]
            , div [] (List.map renderOption story.options)
            , br [] []
            , button [ onClick OpenSave ] [ text "Saves" ]
            , button [ onClick SaveGame ] [ text "Save Current Game" ]
            ]


renderSaves : String -> Saves -> Html Msg
renderSaves currentStory saves =
    let
        cardStyle =
            style
                [ ( "border", "solid 1px black" )
                , ( "padding", "20px" )
                , ( "width", "200px" )
                , ( "height", "100px" )
                , ( "display", "flex" )
                , ( "justify-content", "center" )
                , ( "background-color", "lightgray" )
                , ( "font-size", "1em" )
                , ( "width", "50%" )
                ]

        renderSave : Save -> Html Msg
        renderSave save =
            let
                story : Story
                story =
                    getStory bigStory save
            in
                div [ cardStyle, onClick (Load story.id) ]
                    [ h2 [] [ text story.id ]
                    , p [] [ text story.narrative ]
                    ]
    in
        div [ divStyle ]
            (List.append (List.map renderSave saves) [ button [ onClick (Load currentStory) ] [ text "Return" ] ])


view : Model -> Html Msg
view model =
    case model.mode of
        Game ->
            renderGame (getStory model.entireStory model.currentStory)

        SaveScreen ->
            renderSaves model.currentStory model.saves


update : Msg -> Model -> Model
update msg model =
    case msg of
        Choice id ->
            { model | currentStory = id }

        Load id ->
            { model | currentStory = id, mode = Game }

        OpenSave ->
            { model | mode = SaveScreen }

        SaveGame ->
            if (List.member model.currentStory model.saves) then
                model
            else
                { model | saves = (model.currentStory :: model.saves) }


initialState : Model
initialState =
    { entireStory = bigStory, currentStory = "main", saves = [], mode = Game }


main : Program Never Model Msg
main =
    beginnerProgram
        { view = view
        , update = update
        , model = initialState
        }

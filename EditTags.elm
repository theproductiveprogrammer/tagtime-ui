port module EditTags exposing (main)

import Html
import Html.Attributes as HA
import Html.Events as HE
import Date
import Json.Decode as Decode


{--
        understand/
CSS names are global - defined anywhere they affect each other.

        problem/
We would like to modularize our styles.

        way/
We will use a prefix that is unlikely to be used anywhere else.
-}


class : String -> Html.Attribute msg
class name =
    class2 name ""


class2 : String -> String -> Html.Attribute msg
class2 name otherclasses =
    let
        pfx =
            "tt-edittags-"
    in
        HA.class <| pfx ++ name ++ " " ++ otherclasses



{--
    problem/
Elm does not have a standard way to handle keypress messages. This makes
it hard to do things like accept a user input on pressing "enter".

    way/
Fortunately Elm provides us with a way of simply trapping any DOM event
we like. We will trap the "keydown" event use that ourselves.
-}


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
    HE.on "keydown" (Decode.map tagger HE.keyCode)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { unix : Int
    , tags : List String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { unix = flags.unix, tags = flags.tags, orig = flags.tags }, Cmd.none )


type alias Model =
    { unix : Int
    , tags : List String
    , orig : List String
    }


type Msg
    = Save
    | Cancel
    | UpdateTags String
    | HandleSpecialKeys Int


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


port editdone : List String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Save ->
            save model

        Cancel ->
            cancel model

        UpdateTags t ->
            let
                tags =
                    String.split "," t
            in
                ( { model | tags = tags }, Cmd.none )

        HandleSpecialKeys keycode ->
            handle_special_keys keycode model


save : Model -> ( Model, Cmd Msg )
save model =
    ( model, editdone model.tags )


cancel : Model -> ( Model, Cmd Msg )
cancel model =
    ( model, editdone model.orig )


handle_special_keys : Int -> Model -> ( Model, Cmd Msg )
handle_special_keys keycode model =
    let
        enter_key =
            13

        escape_key =
            27
    in
        if keycode == enter_key then
            save model
        else if keycode == escape_key then
            cancel model
        else
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        b =
            "tt-draggable tt-fullwidth tt-fullheight"

        h =
            model.unix
                |> to_local
                |> Date.hour

        bg =
            if h < 7 || h > 21 then
                "night"
            else if h < 10 then
                "morn"
            else if h < 17 then
                "noon"
            else
                "even"

        c =
            b ++ " tt-bg-" ++ bg
    in
        Html.div [ class2 "container" c ]
            [ prompt_1
            , date_1 model.unix
            , time_1 model.unix
            , input_1 model.tags
            , btns_1
            ]


prompt_1 : Html.Html Msg
prompt_1 =
    Html.div [ class "prompt" ]
        [ Html.text "Tag what you were doing" ]


date_1 : Int -> Html.Html Msg
date_1 unix =
    let
        date =
            unix
                |> to_local
                |> wd
    in
        Html.div [ class "date" ]
            [ Html.text date ]


to_local : Int -> Date.Date
to_local unix =
    Date.fromTime <| toFloat (unix * 1000)


wd : Date.Date -> String
wd date =
    toString (Date.dayOfWeek date)
        ++ " "
        ++ toString (Date.month date)
        ++ "/"
        ++ toString (Date.day date)


time_1 : Int -> Html.Html Msg
time_1 unix =
    let
        time =
            unix
                |> to_local
                |> hhmm
    in
        Html.div [ class "time" ]
            [ Html.text time ]


hhmm : Date.Date -> String
hhmm date =
    pad2 (Date.hour date) ++ ":" ++ pad2 (Date.minute date)


pad2 : Int -> String
pad2 v =
    if v < 10 then
        "0" ++ toString v
    else
        toString v


input_1 : List String -> Html.Html Msg
input_1 tags =
    Html.div [ class "input" ]
        [ Html.input
            [ HE.onInput UpdateTags
            , HA.value <| String.join "," tags
            , class "control"
            , HA.autofocus True
            , onKeyDown HandleSpecialKeys
            ]
            []
        ]


btns_1 : Html.Html Msg
btns_1 =
    Html.div [ class "btns" ]
        [ Html.button [ class "save", HE.onClick Save ] [ Html.text "Save" ]
        , Html.button [ class "cancel", HE.onClick Cancel ] [ Html.text "Cancel" ]
        ]

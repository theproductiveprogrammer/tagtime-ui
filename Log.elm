port module Log exposing (main)

import Html
import Html.Attributes as HA
import Html.Events as HE
import Date
import Task
import Maybe


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
            "tt-log-"
    in
        HA.class <| pfx ++ name ++ " " ++ otherclasses


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { logs : List Log }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            emptyModel
    in
        ( { model | logs = flags.logs }, Task.perform CurrTime Date.now )


type alias Model =
    { now : Int
    , logs : List Log
    , selected : Maybe Int
    , allow_mouse : Bool
    }


emptyModel : Model
emptyModel =
    { now = 0, logs = [], selected = Nothing, allow_mouse = True }


type alias Log =
    { unix : Int
    , tags : List String
    }


type Msg
    = CloseWindow
    | CurrTime Date.Date
    | RequestTagsUpdate Log
    | SaveUpdatedTags ( Int, List String )
    | MouseSelected Int
    | HandleSpecialKeys String
    | EnableMouse String


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ updatedtags SaveUpdatedTags
        , keypressed HandleSpecialKeys
        , mousemoved EnableMouse
        ]


port closewindow : String -> Cmd msg


port updatetags : ( Int, List String ) -> Cmd msg


port updatedtags : (( Int, List String ) -> msg) -> Sub msg


port keypressed : (String -> msg) -> Sub msg



{--
        situation/
We allow the user to select a ping row using either the mouse (by moving
the mouse over an item) or by using the arrow keys.
Now a situation arises where the user has used the mouse to select an
item and then used the arrow keys to move away. When editing the item
another window covers this one and after editing the 'mouseover'
message is re-triggered causing the user's selection to "jump back" to
the row under the mouse.

        problem/
The user is not expecting his selection to suddenly change just because
he has edited another row. It feels 'delicate'.

        way/
We will disable mouse selection once keyboard selection is activate and
only re-enable it when the user moves the mouse.

NB: the "String" parameter is a dummy parameter. Elm wants a parameter
to flow in and we don't care what it is.
-}


port mousemoved : (String -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseWindow ->
            close_window model

        CurrTime d ->
            ( { model | now = round <| Date.toTime d / 1000 }, Cmd.none )

        RequestTagsUpdate log ->
            request_tags_update model log

        SaveUpdatedTags ( unx, tags ) ->
            ( save_updated_tags unx tags model, Cmd.none )

        MouseSelected unix ->
            mouse_selected unix model

        HandleSpecialKeys specialkey ->
            handle_special_keys specialkey model

        EnableMouse _ ->
            ( { model | allow_mouse = True }, Cmd.none )


mouse_selected : Int -> Model -> ( Model, Cmd Msg )
mouse_selected unix model =
    if model.allow_mouse then
        ( { model | selected = Just unix }, Cmd.none )
    else
        ( model, Cmd.none )


close_window : Model -> ( Model, Cmd Msg )
close_window model =
    ( model, closewindow "_" )


request_tags_update : Model -> Log -> ( Model, Cmd Msg )
request_tags_update model log =
    ( model, updatetags ( log.unix, log.tags ) )


handle_special_keys : String -> Model -> ( Model, Cmd Msg )
handle_special_keys specialkey model =
    if specialkey == "Enter" then
        update_selected model
    else if specialkey == "Escape" then
        close_window model
    else if specialkey == "ArrowUp" then
        select_adjacent Up model
    else if specialkey == "ArrowDown" then
        select_adjacent Down model
    else
        ( model, Cmd.none )


type Direction
    = Up
    | Down


select_adjacent : Direction -> Model -> ( Model, Cmd Msg )
select_adjacent direction model =
    let
        logs =
            view_logs model

        nxt unix =
            case direction of
                Up ->
                    logs
                        |> List.filter (\l -> l.unix > unix)
                        |> List.reverse
                        |> List.head

                Down ->
                    logs
                        |> List.filter (\l -> l.unix < unix)
                        |> List.head

        select_first =
            case List.head logs of
                Nothing ->
                    model

                Just log ->
                    { model | selected = Just log.unix }
    in
        case model.selected of
            Nothing ->
                case direction of
                    Down ->
                        ( select_first, Cmd.none )

                    Up ->
                        ( model, Cmd.none )

            Just unix ->
                case nxt unix of
                    Nothing ->
                        ( model, Cmd.none )

                    Just log ->
                        ( { model | selected = Just log.unix, allow_mouse = False }, Cmd.none )


selected_log : Model -> Maybe Log
selected_log model =
    case model.selected of
        Nothing ->
            Nothing

        Just unix ->
            model.logs |> List.filter (\l -> l.unix == unix) |> List.head


update_selected : Model -> ( Model, Cmd Msg )
update_selected model =
    case selected_log model of
        Nothing ->
            ( model, Cmd.none )

        Just log ->
            request_tags_update model log


save_updated_tags : Int -> List String -> Model -> Model
save_updated_tags unx tags model =
    let
        logs =
            List.map
                (\l ->
                    if l.unix == unx then
                        { l | tags = tags }
                    else
                        l
                )
                model.logs
    in
        { model | logs = logs }


view : Model -> Html.Html Msg
view model =
    Html.div
        [ class2 "container" "tt-dragable" ]
        [ drag_strip_1
        , close_btn_1
        , icon_1
        , title_1
        , logs_1 model
        , export_1
        ]


drag_strip_1 : Html.Html Msg
drag_strip_1 =
    Html.div [ class2 "drag" "tt-dragable" ] []


close_btn_1 : Html.Html Msg
close_btn_1 =
    Html.div
        [ class2 "close" "tt-dragable"
        , HE.onClick CloseWindow
        ]
        [ Html.text "X" ]


icon_1 : Html.Html Msg
icon_1 =
    Html.img [ class "icon", HA.src "./icon_32x32.png" ] []


export_1 : Html.Html Msg
export_1 =
    Html.img [ class "export", HA.src "./export-28.png" ] []


title_1 : Html.Html Msg
title_1 =
    Html.div [ class2 "title-row" "tt-dragable" ]
        [ Html.span [ class2 "title" "tt-dragable" ]
            [ Html.text "YOUR LOG" ]
        ]


view_logs : Model -> List Log
view_logs model =
    model.logs
        |> List.filter (\l -> l.unix <= model.now)
        |> List.reverse


logs_1 : Model -> Html.Html Msg
logs_1 model =
    let
        logs =
            view_logs model

        split_for_each_day =
            day_split_1 logs
    in
        Html.div [ class "container-logs" ]
            (List.map (log_day_1 model.selected) split_for_each_day)


day_split_1 : List Log -> List (List Log)
day_split_1 logs =
    List.map (\l -> List.filter (day_match l) logs) <| uniq logs


day_match : Log -> Log -> Bool
day_match l1 l2 =
    let
        e1 =
            unix_to_local l1.unix

        e2 =
            unix_to_local l2.unix
    in
        Date.year e1
            == Date.year e2
            && Date.month e1
            == Date.month e2
            && Date.day e1
            == Date.day e2


uniq : List Log -> List Log
uniq logs =
    uniq_1 logs []


uniq_1 : List Log -> List Log -> List Log
uniq_1 logs accum =
    case logs of
        [] ->
            List.reverse accum

        h :: t ->
            let
                in_accum a e =
                    not (List.isEmpty <| List.filter (day_match e) a)
            in
                if in_accum accum h then
                    uniq_1 t accum
                else
                    uniq_1 t (h :: accum)


log_day_1 : Maybe Int -> List Log -> Html.Html Msg
log_day_1 selected logs =
    Html.div [ class "log-row" ]
        [ day_title_1 (List.head logs)
        , day_pings_1 selected logs
        ]


day_title_1 : Maybe Log -> Html.Html Msg
day_title_1 log =
    let
        htm =
            case log of
                Nothing ->
                    Html.span [] []

                Just l ->
                    Html.div [ class "date-title" ]
                        [ Html.div [ class "wd-title" ]
                            [ Html.text (wd l.unix) ]
                        , Html.div [ class "md-title" ]
                            [ Html.text (md l.unix) ]
                        ]
    in
        Html.div [ class "date-title-col" ] [ htm ]


day_pings_1 : Maybe Int -> List Log -> Html.Html Msg
day_pings_1 selected logs =
    Html.div [ class "pings-col" ] (List.map (day_ping_1 selected) logs)


day_ping_1 : Maybe Int -> Log -> Html.Html Msg
day_ping_1 selected log =
    let
        c =
            if log.unix == Maybe.withDefault 0 selected then
                "ping-selected"
            else
                "ping"
    in
        Html.div
            [ class c
            , HE.onClick (RequestTagsUpdate log)
            , HE.onMouseEnter (MouseSelected log.unix)
            ]
            [ Html.div [ class2 "time" "tt-side-by-side" ] [ Html.text (hhmm log.unix) ]
            , Html.div [ class2 "tags" "tt-side-by-side" ] [ Html.text <| String.join ", " log.tags ]
            ]


wd : Int -> String
wd unix =
    toString (Date.dayOfWeek <| unix_to_local unix)


md : Int -> String
md unix =
    let
        date =
            unix_to_local unix
    in
        toString (Date.month date) ++ " / " ++ toString (Date.day date)


hhmm : Int -> String
hhmm unix =
    let
        date =
            unix_to_local unix
    in
        pad2 (Date.hour date)
            ++ ":"
            ++ pad2 (Date.minute date)


pad2 : Int -> String
pad2 v =
    if v < 10 then
        "0" ++ toString v
    else
        toString v


unix_to_local : Int -> Date.Date
unix_to_local unix =
    Date.fromTime <| toFloat (unix * 1000)

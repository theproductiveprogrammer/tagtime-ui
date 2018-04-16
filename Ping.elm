port module Ping exposing (main)

import Html
import Html.Attributes as HA
import Html.Events as HE
import Http
import Svg
import Svg.Attributes as SA
import Date
import Json.Decode as Decode
import Set
import Task
import Dom
import Dom.Scroll as Scroll


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
    HA.class (classpfx name)


classpfx : String -> String
classpfx name =
    "tt-ping-" ++ name


{-|
        understand/
Main entry point of our program. Note that here we only declare our
framework entry functions and do nothing else
-}
main : Program Consts Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-|
        understand/
These constants are supplied by the electron main process
-}
type alias Consts =
    { server_url : String
    , uuid : String
    }


{-|
        understand/
The ping schedule for the user for the next week or so
-}
type alias Sch =
    Int


{-|
        understand/
A category groups the tags to make it easier to remember what kinds of
data needs to be entered. In general categories match the the 5 W's:
    + Who: "People" category
    + What: "Activity" category
    + When: <autocaptured - ping time>
    + Where: "Location" category
    + Why: "Project" category
(See the server code for a more detailed discussion on Categories)
-}
type alias Category =
    { name : String
    , icon : String
    , tags : List String
    }


{-|
        understand/
A 'Ping' represents a sampling of what the user was doing at a given
unix time.
-}
type alias Ping =
    { unix : Int
    , tags : List String
    }


{-|
        understand/
The main model of TEA - The Elm Architecture. Everything happening in
our system (except for CSS animations) can be derived from this model so
it will have a hodge-podge of everything we need to keep track of.
-}
type alias Model =
    { consts : Consts
    , sch : List Sch
    , svrcats : List Category
    , cats : List Category
    , pings : List Ping
    , login_req_in_flight : Bool
    , after_login : List (Cmd Msg)
    }


{-|
        outcome/
Initialize our model with the constants given by electron
-}
init : Consts -> ( Model, Cmd Msg )
init consts =
    ( { consts = consts
      , sch = []
      , svrcats = []
      , cats = []
      , pings = []
      , login_req_in_flight = False
      , after_login = []
      }
    , getData consts
    )


getData : Consts -> Cmd Msg
getData consts =
    Cmd.batch [ getSch consts, getCats consts, getPings consts ]


{-|
        understand/
In addition to user interaction and HTTP events, we can "subscribe" to
push events - timers and websockets.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



{--
        understand/
Messages are how the model gets updated. They are clean and structured
way to centralize all changes. This gives us the following benefits:
1. No changes happen somewhere deep inside a code flow
2. Fewer unexpected side effects happen when we change or remove a
function because coupling should be reduced.
-}


type Msg
    = SaveNewSch (Result Http.Error (List Sch))
    | SaveNewCats (Result Http.Error (List Category))
    | SaveNewPings (Result Http.Error (List Ping))
    | AfterLogin (Result Http.Error ())
    | PingSheetScrolled Float
    | IgnoreScrollSync (Result Dom.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IgnoreScrollSync _ ->
            ( model, Cmd.none )

        SaveNewSch r ->
            saveNewSch r model

        SaveNewCats r ->
            saveNewCats r model

        SaveNewPings r ->
            saveNewPings r model

        AfterLogin r ->
            afterLogin r model

        PingSheetScrolled scrollTop ->
            ping_sheet_scrolled scrollTop model


{-|
        outcome/
Simple HTTP PUT request to our API
-}
put : String -> Http.Body -> Http.Request ()
put url body =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


{-|
        outcome/
Helper function to construct a URL for the given api call
-}
apiURL : Consts -> String -> String
apiURL consts api =
    let
        apiURL =
            consts.server_url ++ "/" ++ api
    in
        apiURL ++ "?u=" ++ Http.encodeUri consts.uuid


{-|
        outcome/
Helper function to construct a URL for the given image
-}
imgURL : Consts -> String -> String
imgURL consts img =
    consts.server_url ++ img


{-|
        situation/
The user has pings that contain tags and categories under which tags are
grouped. While all the tags should be grouped under appropriate
categories it is quite possible that there are tags entered that do not
fall under any category.

        problem/
Our interface displays tags in swim-lanes under each category. Tags that
are not under any category will not be displayed.

        way/
We create a new category "uncategorised/?" and put all the uncategorised
tags under that. We do this every time we receive updated categories or
pings from the server.
-}
classifyAllTags : Model -> Model
classifyAllTags model =
    let
        extract_tag_set a =
            Set.fromList <| List.concat <| List.map .tags <| List.take 500 a

        all_tag_set =
            extract_tag_set model.pings

        svrcat_tag_set =
            extract_tag_set model.svrcats

        uncategorized_tags =
            Set.toList <| Set.diff all_tag_set svrcat_tag_set

        un_cat_icon =
            "/r/noun_101469_cc.png"

        un_cat =
            Category "(?)" un_cat_icon uncategorized_tags

        cats =
            List.reverse <| un_cat :: List.reverse model.svrcats
    in
        { model | cats = cats }


{-|
        outcome/
Request the server for the latest ping schedule.
-}
getSch : Consts -> Cmd Msg
getSch consts =
    let
        url =
            apiURL consts "api/sch"

        req =
            Http.get url decodeSch
    in
        Http.send SaveNewSch req


decodeSch : Decode.Decoder (List Sch)
decodeSch =
    Decode.list Decode.int


{-|
        outcome/
Process the latest ping schedule returned by the server
-}
saveNewSch : Result Http.Error (List Sch) -> Model -> ( Model, Cmd Msg )
saveNewSch r model =
    case r of
        Ok sch ->
            ( { model | sch = sch }, Cmd.none )

        Err err ->
            onHttpError err model (getSch model.consts)


{-|
        outcome/
Request the server for the latest category information
-}
getCats : Consts -> Cmd Msg
getCats consts =
    let
        url =
            apiURL consts "api/cat"

        req =
            Http.get url decodeCats
    in
        Http.send SaveNewCats req


decodeCats : Decode.Decoder (List Category)
decodeCats =
    let
        decodeCat =
            Decode.map3 Category
                (Decode.field "name" Decode.string)
                (Decode.field "icon" Decode.string)
                (Decode.field "tags" (Decode.list Decode.string))
    in
        Decode.list decodeCat


{-|
        outcome/
Process the latest category data returned by the server
-}
saveNewCats : Result Http.Error (List Category) -> Model -> ( Model, Cmd Msg )
saveNewCats r model =
    case r of
        Ok cats ->
            ( classifyAllTags { model | svrcats = cats }, Cmd.none )

        Err err ->
            onHttpError err model (getCats model.consts)


{-|
        outcome/
Request the server for the latest ping data
-}
getPings : Consts -> Cmd Msg
getPings consts =
    let
        url =
            apiURL consts "api/ping"

        req =
            Http.get url decodePings
    in
        Http.send SaveNewPings req


decodePings : Decode.Decoder (List Ping)
decodePings =
    let
        decodePing =
            Decode.map2 Ping
                (Decode.field "unix" Decode.int)
                (Decode.field "tags" (Decode.list Decode.string))
    in
        Decode.list decodePing


{-|
        outcome/
Process the latest ping data returned by the server
-}
saveNewPings : Result Http.Error (List Ping) -> Model -> ( Model, Cmd Msg )
saveNewPings r model =
    case r of
        Ok pings ->
            ( classifyAllTags { model | pings = pings }, Cmd.none )

        Err err ->
            onHttpError err model (getPings model.consts)



{--
        outcome/
We wanted to perform an action but the server refused so now we're
trying to login and perform that action after that.
Because this can be called multiple times - all api's fail when we
aren't logged in - we keep track of the fact we've already made a
login request and don't try to do it again until it's complete.
-}


onHttpError : Http.Error -> Model -> Cmd Msg -> ( Model, Cmd Msg )
onHttpError err model cmd =
    case err of
        Http.BadStatus resp ->
            if resp.status.code == httpStatusForbidden then
                loginThen cmd model
            else
                ( Debug.log (toString err) model, Cmd.none )

        _ ->
            ( Debug.log (toString err) model, Cmd.none )


loginThen : Cmd Msg -> Model -> ( Model, Cmd Msg )
loginThen msg model =
    let
        url =
            apiURL model.consts "user"

        req =
            put url Http.emptyBody

        m =
            { model | after_login = msg :: model.after_login }
    in
        if m.login_req_in_flight then
            ( m, Cmd.none )
        else
            ( { m | login_req_in_flight = True }, Http.send AfterLogin req )


{-|
        situtation/
We have attempted an API call and the server replied saying we aren't
logged in. So we need to login first then try again.

        outcome/
If we have logged in successfully, we retry all the calls that failed
because we weren't logged in. We also turn off the flag that told us
login was in flight.
-}
afterLogin : Result Http.Error () -> Model -> ( Model, Cmd Msg )
afterLogin r model =
    let
        m =
            { model | login_req_in_flight = False }
    in
        case r of
            Ok _ ->
                ( m, Cmd.batch model.after_login )

            Err err ->
                ( Debug.log (toString err) m, Cmd.none )



{--
        understand/
This is the "view/rendering" portion of TEA - The (wonderful) Elm
Architecture. Here is where we actually take in the model and construct
a "snapshot" of what it should look like. Elm then takes this snapshot,
compares it with what is actually shown, and updates the view with the
changes automagically. It's a lovely simplification that allows us to
code without worrying about anything except the current state of the
model.

        siblings/
* React.js
* Vue.js

-}


view : Model -> Html.Html Msg
view model =
    Html.div [ class "container" ]
        [ friction_bar
        , calendar_bar model.sch
        , category_pane model.consts model.cats
        , ping_sheet model.cats model.pings
        , bottom_slit
        ]


friction_bar : Html.Html Msg
friction_bar =
    let
        style =
            HA.style
                [ ( "background-image", "url(ping-friction-bar.png)" )
                , ( "background-repeat", "no-repeat" )
                , ( "background-attachment", "fixed" )
                , ( "background-position", "center -3px" )
                , ( "width", "100%" )
                , ( "height", "64px" )
                ]
    in
        Html.div [ style, HA.class "tt-dragable" ] []


bottom_slit : Html.Html Msg
bottom_slit =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px (cat_pane_top + cat_pane_height) )
                , ( "left", px cat_pane_left )
                , ( "width", px (cat_pane_width + calendar_width) )
                , ( "height", "4px" )
                , ( "box-shadow", "inset 0px -5px 4px -5px #000" )
                ]
    in
        Html.div [ style ] []



{--
        understand/
The calendar is the top scrolling bar that shows the pings with their
date/times
-}


calendar_width : Int
calendar_width =
    700


calendar_height : Int
calendar_height =
    100


calendar_bar : List Sch -> Html.Html Msg
calendar_bar sch =
    let
        style =
            HA.style
                [ ( "width", px calendar_width )
                , ( "height", px calendar_height )
                , ( "position", "fixed" )
                , ( "right", "0" )
                ]
    in
        Html.div [ style ]
            [ calendar_end_marker
            , calendar_sch sch
            ]


calendar_end_marker : Html.Html Msg
calendar_end_marker =
    let
        y1 =
            toString (calendar_height // 4)

        y2 =
            toString ((calendar_height // 4) * 3)

        left_bar =
            Svg.line
                [ SA.stroke "black"
                , SA.strokeWidth "2"
                , SA.x1 "1"
                , SA.x2 "1"
                , SA.y1 y1
                , SA.y2 y2
                ]
                []

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "left", "0px" )
                ]
    in
        Svg.svg
            [ style
            , SA.width "2"
            , SA.height "100"
            , SA.viewBox "0 0 2 100"
            ]
            [ left_bar ]


calendar_sch : List Sch -> Html.Html Msg
calendar_sch sch =
    let
        top =
            (calendar_height // 2) - 5

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", "0" )
                , ( "padding-top", "6px" )
                ]

        scrollstyle =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px top )
                , ( "width", px calendar_width )
                , ( "height", "3.1em" )
                , ( "overflow-x", "scroll" )
                , ( "overflow-y", "hidden" )
                ]

        ndx =
            List.range 0 (List.length sch)
    in
        Html.div [ scrollstyle, HA.class "tt-no-scrollbar" ]
            [ Html.div [ style ] (List.map2 calendar_tick ndx sch) ]


cell_width : Int
cell_width =
    64


calendar_tick : Int -> Sch -> Html.Html Msg
calendar_tick ndx sch =
    let
        time_display =
            hhmm sch |> Html.text

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "left", px (cell_width * ndx) )
                , ( "width", px cell_width )
                ]
    in
        Html.div [ class "calendar-tick", style ]
            [ Html.span [ class "calendar-time" ] [ time_display ] ]



{--
        understand/
The category pane groups all the tags under that category to make it
easier to remember what kinds of data needs to be entered
-}


cat_pane_width : Int
cat_pane_width =
    262


cat_pane_height : Int
cat_pane_height =
    350


cat_panel_min_height : Int
cat_panel_min_height =
    180


cat_pane_top : Int
cat_pane_top =
    170


cat_pane_left : Int
cat_pane_left =
    8


cat_panel_colors : List String
cat_panel_colors =
    {--[ "#FBECD0", "#D2F6F3", "#F6F3D2", "#F6D2D7" ] --}
    [ "#F6E0D2", "#D2F6F3", "#F6F3D2", "#F6D2D7" ]



{--
        outcome/
The category pane shows each of the categories in it's own panel. We
rotate between four background colors for each panel.
-}


cat_pane_id : String
cat_pane_id =
    "tt-ping-cat-pane"


category_pane : Consts -> List Category -> Html.Html Msg
category_pane consts cats =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px cat_pane_top )
                , ( "left", px cat_pane_left )
                , ( "width", px cat_pane_width )
                , ( "height", px cat_pane_height )
                , ( "overflow-x", "hidden" )
                , ( "overflow-y", "scroll" )
                ]

        colors =
            prop_list cat_panel_colors (List.length cats)
    in
        Html.div [ HA.id cat_pane_id, style ]
            (List.map2 (category_panel consts) cats colors)


cat_panel_height : Category -> Int
cat_panel_height cat =
    category_pane_padding + (List.length cat.tags + 1) * (row_height + 1)


category_panel : Consts -> Category -> String -> Html.Html Msg
category_panel consts cat color =
    let
        style =
            HA.style
                [ ( "position", "relative" )
                , ( "width", px cat_pane_width )
                , ( "min-height", px cat_panel_min_height )
                , ( "height", px (cat_panel_height cat) )
                , ( "background-color", color )
                , ( "border-bottom-left-radius", "8px" )
                , ( "border-top-left-radius", "8px" )
                , ( "box-shadow", "2px 1px 2px 1px #bbb" )
                , ( "margin-bottom", "20px" )
                ]
    in
        Html.div [ style ]
            [ category_name consts cat
            , category_tags cat
            ]


category_pane_padding : Int
category_pane_padding =
    10


category_pane_name_width : Int
category_pane_name_width =
    120


category_name : Consts -> Category -> Html.Html Msg
category_name consts cat =
    let
        icon =
            imgURL consts cat.icon

        iconstyle =
            HA.style
                [ ( "width", "85px" )
                , ( "height", "85px" )
                , ( "text-align", "center" )
                ]

        titlestyle =
            HA.style
                [ ( "font-size", "24px" )
                ]

        style =
            HA.style
                [ ( "width", px category_pane_name_width )
                , ( "padding-top", px category_pane_padding )
                , ( "padding-left", px category_pane_padding )
                ]
    in
        Html.div [ style ]
            [ Html.img [ iconstyle, HA.src icon ] []
            , Html.div [ titlestyle ] [ Html.text cat.name ]
            ]


tag_line_opacity : Int -> String
tag_line_opacity ndx =
    if ndx % 2 == 0 then
        "0.85"
    else
        "0.65"


{-|
        outcome/
The category tags are shown in each category. Each tag line has a
different background color to distinguish rows.
-}
category_tags : Category -> Html.Html Msg
category_tags cat =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px category_pane_padding )
                , ( "left", px (category_pane_name_width + category_pane_padding) )
                ]

        ndx =
            List.range 0 (List.length cat.tags)
    in
        Html.div [ style ]
            (List.map2 category_tag ndx cat.tags)


row_height : Int
row_height =
    32


row_border_color : String
row_border_color =
    "#ddd"


cat_tag_width : Int
cat_tag_width =
    200


category_tag : Int -> String -> Html.Html Msg
category_tag ndx tag =
    let
        linestyle =
            HA.style
                [ ( "position", "relative" )
                , ( "overflow", "hidden" )
                , ( "height", px row_height )
                , ( "line-height", px row_height )
                , ( "width", px cat_tag_width )
                , ( "border-bottom", "1px solid " ++ row_border_color )
                , ( "padding-left", "6px" )
                , ( "font-size", "14px" )
                ]

        bgstyle =
            HA.style
                [ ( "position", "absolute" )
                , ( "left", "0" )
                , ( "top", "0" )
                , ( "background-color", "white" )
                , ( "border-top-left-radius", "2px" )
                , ( "border-bottom-left-radius", "2px" )
                , ( "opacity", tag_line_opacity ndx )
                , ( "height", px row_height )
                , ( "width", px cat_tag_width )
                ]
    in
        Html.div [ linestyle ]
            [ Html.div [ bgstyle ] []
            , Html.div [ linestyle ] [ Html.text tag ]
            ]



{--
        understand/
Pings are shown in a visual sheet with each tag occupying it's own
swimlane. The ping sheet is positioned flush against the category pane
and has the width of the calendar.
-}


ping_sheet : List Category -> List Ping -> Html.Html Msg
ping_sheet cats pings =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px cat_pane_top )
                , ( "left", px (cat_pane_width + cat_pane_left) )
                , ( "width", px calendar_width )
                , ( "height", px cat_pane_height )
                , ( "overflow-x", "scroll" )
                , ( "overflow-y", "scroll" )
                ]

        colors =
            prop_list cat_panel_colors (List.length cats)
    in
        Html.div [ style, onScroll PingSheetScrolled ]
            (List.map2 (ping_catlane pings) cats colors)


ping_sheet_scrolled : Float -> Model -> ( Model, Cmd Msg )
ping_sheet_scrolled scrollTop model =
    ( model, Task.attempt IgnoreScrollSync <| Scroll.toY cat_pane_id scrollTop )


{-|
            outcome/
In order to scroll correctly we need to set the ping width based on the
number of pings
-}
ping_width : List Ping -> Int
ping_width pings =
    cell_width * (List.length pings + 1)


ping_catlane : List Ping -> Category -> String -> Html.Html Msg
ping_catlane pings cat color =
    let
        style =
            HA.style
                [ ( "position", "relative" )
                , ( "width", px <| ping_width pings )
                , ( "min-height", px cat_panel_min_height )
                , ( "height", px (cat_panel_height cat) )
                , ( "background-color", color )
                , ( "box-shadow", "2px 1px 2px 1px #bbb" )
                , ( "margin-bottom", "20px" )
                ]
    in
        Html.div [ style ]
            [ ping_taglanes pings cat ]


ping_taglanes : List Ping -> Category -> Html.Html Msg
ping_taglanes pings cat =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px category_pane_padding )
                , ( "left", "0" )
                , ( "width", px <| ping_width pings )
                ]

        ndx =
            List.range 0 (List.length cat.tags)
    in
        Html.div [ style ]
            (List.map2 (ping_taglane pings) ndx cat.tags)


ping_taglane : List Ping -> Int -> String -> Html.Html Msg
ping_taglane pings ndx tag =
    let
        linestyle =
            HA.style
                [ ( "position", "relative" )
                , ( "overflow", "hidden" )
                , ( "height", px row_height )
                , ( "line-height", px row_height )
                , ( "width", px <| ping_width pings )
                , ( "background-color", "white" )
                , ( "opacity", tag_line_opacity ndx )
                , ( "padding-left", "6px" )
                , ( "border-bottom", "1px solid " ++ row_border_color )
                , ( "font-size", "14px" )
                ]

        ping_ndx =
            List.range 0 (List.length pings)
    in
        Html.div [ linestyle ]
            (List.foldr
                (\( ndx, ping ) acc ->
                    if List.member tag ping.tags then
                        ping_tag_cell ndx :: acc
                    else
                        acc
                )
                []
                (List.map2 (,) ping_ndx pings)
            )


ping_tag_cell : Int -> Html.Html Msg
ping_tag_cell ndx =
    if ndx == 0 then
        ping_tag_first_cell
    else
        ping_tag_inside_cell ndx


ping_tag_first_cell : Html.Html Msg
ping_tag_first_cell =
    let
        height =
            row_height - 16

        radius =
            height // 2

        width =
            cell_width + radius

        top =
            (row_height - height) // 2

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "height", px height )
                , ( "width", px width )
                , ( "left", "0" )
                , ( "top", px top )
                , ( "border-radius", px radius )
                , ( "background-color", "black" )
                ]
    in
        Html.div [ style ] []


ping_tag_inside_cell : Int -> Html.Html Msg
ping_tag_inside_cell ndx =
    let
        height =
            row_height - 16

        radius =
            height // 2

        width =
            cell_width + 2 * radius

        left =
            (cell_width * ndx) - radius

        top =
            (row_height - height) // 2

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "height", px height )
                , ( "width", px width )
                , ( "left", px left )
                , ( "top", px top )
                , ( "border-radius", px radius )
                , ( "background-color", "black" )
                ]
    in
        Html.div [ style ] []



{--
        understand/
View helper functions
-}


px : Int -> String
px v =
    (toString v) ++ "px"


pad2 : Int -> String
pad2 v =
    if v < 10 then
        "0" ++ toString v
    else
        toString v


unix_to_local : Int -> Date.Date
unix_to_local unix =
    Date.fromTime <| toFloat (unix * 1000)


hhmm : Int -> String
hhmm unix =
    let
        date =
            unix_to_local unix
    in
        pad2 (Date.hour date)
            ++ ":"
            ++ pad2 (Date.minute date)


httpStatusForbidden : Int
httpStatusForbidden =
    403



{--
        outcome/
We need to be able to capture scrolling events
-}


onScroll : (Float -> msg) -> Html.Attribute msg
onScroll tagger =
    HE.on "scroll" <| Decode.map tagger scrollTop


scrollTop : Decode.Decoder Float
scrollTop =
    Decode.at [ "target", "scrollTop" ] Decode.float


{-|
        problem/
In a UI, there are many places were we would like lists of data items to
stand out with different properties (color/opacity etc).
However, because the items are dynamic (coming from data) we don't have
a way to hard-code it into the interface.

        way/
We will take a list of properties and rotate them among the data item -
giving each item a color from the list. To do this, we create a parallel
list so that each item in the original list can be zipped up with a
corresponding color from this list.

        --examples/
An instance of the data list has 10 items and we have 3 colors:
    [ item_1 item_2 item_3 item_4 item_5 ... item_10 ]
    [ color1 color2 color3 ]

We make (10/3 = 3.3 ~ 4) copies of the list
    [ [ color1 color2 color3 ] [ color1 ... [ color1 color2 color3 ] ]
and then "flatten" them out:
    [ color1 color2 color3 color1 color2 ... color1 color2 color3 ]

Now we have two lists
    [ item_1 item_2 item_3 item_4 item_5 ... item_10 ]
    [ color1 color2 color3 color1 color2 ... color1 color2 color3 ]

and using `zip/map2` (which drops extra elements from the larger list)
we can use the corresponding color for the corresponding element.

        steps/
1. Calculate (data_len // len(colors)) + 1 ==> num_repeats
2. List.repeat colors num_repeats ===> repeated_colors
3. List.concat repeated_colors ===> correponding_colors_list

-}
prop_list : List a -> Int -> List a
prop_list props data_len =
    let
        props_len =
            List.length props

        num_repeats =
            if props_len > 0 then
                (data_len // props_len) + 1
            else
                1
    in
        List.concat <| List.repeat num_repeats props

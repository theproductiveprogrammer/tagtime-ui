port module Ping exposing (main)

import Html
import Html.Attributes as HA
import Html.Events as HE
import Http
import Date
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing (Set)
import Time exposing (Time)


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
    HA.class (classpfx_1 name)


classpfx_1 : String -> String
classpfx_1 name =
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
We need to keep track of the status of the item:
    a. Obtained from Server
    b. Updated in memory
    c. In-flight to server
    e. Server Accepted == Obtained from Server
    f. Server Rejected
TODO: save locally as well so it works offline
-}
type SyncStatus
    = OnServer
    | InMemory
    | InFlight
    | ServerSaysNo


type alias SyncAble a =
    { a | saved : SyncStatus }


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
    , recent : List String
    , alpha : List String
    , sort_order : Int
    , rename : String
    , saved : SyncStatus
    }


{-|
        problem/
When editing the category we have to keep track of the following:
    1. The category name changes
    2. A tag in the category has been renamed
    3. A tag in the category has it's category changed
This information cannot be easily 'derived' from the curreent model
category data.

        way/
We keep a separate data structure which fits our requirements more
naturally and convert back and forth from category to this data
structure.
-}
type alias EditingCat =
    { name : String
    , rename : String
    , icon : String
    , tags : List CatTag
    , newtag : String
    }


type alias CatTag =
    { name : String
    , rename : String
    , cat : String
    }


{-|
        situation/
The user has selected multiple items (pings) and we need to know if a
given element (tag) is present in them.

        understand/
It could be present in all of the items, in some of them, or in none. If
there is no selection at all we cannot reasonably say anything so we say
that there is no selection.
-}
type SelStatus
    = InAll
    | InSome
    | InNone
    | NoSelection


{-|
        understand/
A 'Ping' represents a sampling of what the user was doing at a given
unix time.
-}
type alias Ping =
    { unix : Int
    , tags : List String
    , saved : SyncStatus
    }


type Dialog
    = NotDone
    | CatEdit


{-|
        understand/
The main model of TEA - The Elm Architecture. Everything happening in
our system (except for CSS animations) can be derived from this model so
it will have a hodge-podge of everything we need to keep track of.
-}
type alias Model =
    { consts : Consts
    , sch : List Sch
    , cats : List Category
    , pings : List Ping
    , get_sch_failed : Bool
    , get_cats_failed : Bool
    , get_pings_failed : Bool
    , current : Set Int
    , uncategorized : List String
    , dialog : Maybe Dialog
    , after_login : List (Cmd Msg)
    , last_tick : Time
    , login_req_in_flight : Bool
    , editing_cat : Maybe EditingCat
    }



{--
        understand/
Helper functions for dealing with the model data
-}


pingFor : Int -> Ping
pingFor unix =
    Ping unix [] InMemory


getPing : Model -> Int -> Ping
getPing model unx =
    List.foldr
        (\ping acc ->
            if ping.unix == unx then
                ping
            else
                acc
        )
        (pingFor unx)
        model.pings


getCat : Model -> String -> Maybe Category
getCat model name =
    List.head <|
        List.filter (\c -> strEq c.name name) model.cats


getCatForTag : Model -> String -> Maybe Category
getCatForTag model tag =
    List.foldr
        (\cat acc ->
            if member tag cat.tags then
                Just cat
            else
                acc
        )
        Nothing
        model.cats


{-|
        outcome/
Check if the given tag is in the current selection and return the status
-}
tagInSelection : Model -> String -> SelStatus
tagInSelection model tag =
    let
        sel_pings =
            Set.toList model.current
                |> List.map (getPing model)

        in_ping p =
            List.foldr
                (\t acc -> acc || strEq t tag)
                False
                p.tags

        in_all_sels =
            List.foldr (\p acc -> acc && in_ping p) True sel_pings

        in_any_sel =
            List.foldr (\p acc -> acc || in_ping p) False sel_pings
    in
        if List.length sel_pings == 0 then
            NoSelection
        else if in_all_sels then
            InAll
        else if in_any_sel then
            InSome
        else
            InNone


{-|
        situation/
The user has renamed something - a category or a tag.

        problem/
Now if we just go ahead and rename it, it will go out of sync with the
server is not informed of this rename.

        way/
We will keep track of the rename and the original name on the server
until the server has been informed and then we'll updated it.
-}
currName : { a | name : String, rename : String } -> String
currName a =
    let
        rename =
            String.trim a.rename
    in
        if rename == "" then
            String.trim a.name
        else
            rename


{-|
        outcome/
Create an EditingCat so we can edit a category
-}
editingCatFrom : Model -> Category -> EditingCat
editingCatFrom model cat =
    let
        cat_name t =
            case getCatForTag model t of
                Nothing ->
                    ""

                Just c ->
                    c.name

        cat_tag_from t =
            CatTag t "" (cat_name t)

        cat_tags =
            List.map cat_tag_from cat.tags
    in
        EditingCat cat.name cat.rename cat.icon cat_tags ""


{-|
        outcome/
Converts the EditingCat information in the model into edited
categories and renamed tags.
-}
fromEditingCat : Model -> EditingCat -> ( List Category, List ( String, String ) )
fromEditingCat model ecat =
    let
        m =
            if ecat.name == uncategorized_name then
                model
            else
                update_cat_info_1 model ecat
    in
        ( add_in_cat_tags_1 m ecat.tags, get_renamed_tags_1 ecat )


update_cat_info_1 : Model -> EditingCat -> Model
update_cat_info_1 model ecat =
    let
        tags =
            List.map .rename <|
                List.filter (\ct -> strEq ct.cat ecat.name) ecat.tags

        cat =
            getCat model ecat.name
    in
        case cat of
            Nothing ->
                Debug.log "Unexpected Error: 843" model

            Just c ->
                let
                    u =
                        { c
                            | rename = ecat.rename
                            , icon = ecat.icon
                            , tags = []
                            , saved = InMemory
                        }

                    cats =
                        List.map
                            (\cat ->
                                if strEq cat.name u.name then
                                    u
                                else
                                    cat
                            )
                            model.cats
                in
                    { model | cats = cats }


add_in_cat_tags_1 : Model -> List CatTag -> List Category
add_in_cat_tags_1 model tags =
    List.foldr
        (\tag accum ->
            List.map
                (\cat ->
                    if strEq cat.name tag.cat then
                        let
                            tgs =
                                (currName tag) :: cat.tags
                        in
                            cleanTags model { cat | tags = tgs, saved = InMemory }
                    else
                        cat
                )
                accum
        )
        model.cats
        tags


get_renamed_tags_1 : EditingCat -> List ( String, String )
get_renamed_tags_1 ecat =
    List.foldr
        (\tag accum ->
            let
                rename =
                    String.trim tag.rename
            in
                if String.length rename > 0 then
                    ( tag.name, rename ) :: accum
                else
                    accum
        )
        []
        ecat.tags


{-|
        outcome/
Treat strings that people expect to be "the same" as equal rather than
doing a "computer-like" character comparison. This applies to tags and
categories.
-}
strEq : String -> String -> Bool
strEq t1 t2 =
    let
        tag1 =
            normalize_str_1 t1

        tag2 =
            normalize_str_1 t2
    in
        tag1 == tag2


normalize_str_1 : String -> String
normalize_str_1 t =
    String.toLower <| String.trim t


{-|
        outcome/
Checks if the given string is a member of the list (using strEq)
-}
member : String -> List String -> Bool
member s l =
    not <| List.isEmpty <| List.filter (\e -> strEq e s) l


{-|
        outcome/
Append the given element if not already present in the list
-}
addNew : String -> List String -> List String
addNew s l =
    let
        v =
            String.trim s
    in
        if v == "" || member v l then
            l
        else
            v :: l


{-|
        understand/
Because we are looking at merging potentially huge lists a naive
implementation runs into the problem of "stack overflow". To help this
out we create a helper function that takes an accumulator which helps
the elm compiler to implement "tail-call optimization" and just drop the
stack frames. This added complexity is where an iterative solution
clearly wins over a functional one but - as we only use it when
optimization is needed - we can learn to live with the added complexity.

        problem/
As we refresh data from the server we run into all the usual glorious
issues with keeping things in sync.

        way/
Sync is painful and, with our knowledge of distributed systems, we now
know there is no general solution. For our case, however, we can make
some simplifying examples and that should get us a reasonable outcome.

    1. If we have modified the data (data in-memory or in-flight) we
    assume that's the latest version and don't update it.
    2. If the data is from the server, we update it.
    3. If the data has been rejected by the server, we assume the server
    has a good reason so we force-update it. This means we need to
    make sure we inform the user about server rejections otherwise she
    will assume it has succeeded when it hasn't.
-}
sync_merge : (SyncAble a -> SyncAble a -> Order) -> List (SyncAble a) -> List (SyncAble a) -> List (SyncAble a)
sync_merge cmp my svr =
    sync_merge_1 cmp my svr []


sync_merge_1 : (SyncAble a -> SyncAble a -> Order) -> List (SyncAble a) -> List (SyncAble a) -> List (SyncAble a) -> List (SyncAble a)
sync_merge_1 cmp my svr res =
    case ( my, svr ) of
        ( _, [] ) ->
            List.append res my

        ( [], _ ) ->
            List.append res svr

        ( h_my :: t_my, h_svr :: t_svr ) ->
            case cmp h_my h_svr of
                LT ->
                    sync_merge_1 cmp t_my svr (h_my :: res)

                GT ->
                    sync_merge_1 cmp my t_svr (h_svr :: res)

                EQ ->
                    case h_my.saved of
                        OnServer ->
                            sync_merge_1 cmp t_my t_svr (h_svr :: res)

                        InMemory ->
                            sync_merge_1 cmp t_my t_svr (h_my :: res)

                        InFlight ->
                            sync_merge_1 cmp t_my t_svr (h_my :: res)

                        ServerSaysNo ->
                            sync_merge_1 cmp t_my t_svr (h_svr :: res)


{-|
        outcome/
Initialize our model with the constants given by electron
-}
init : Consts -> ( Model, Cmd Msg )
init consts =
    let
        get_data =
            Cmd.batch [ getSch consts, getCats consts, getPings consts ]
    in
        ( { consts = consts
          , sch = []
          , cats = []
          , pings = []
          , get_sch_failed = False
          , get_cats_failed = False
          , get_pings_failed = False
          , current = Set.empty
          , uncategorized = []
          , dialog = Nothing
          , after_login = []
          , last_tick = 0
          , login_req_in_flight = False
          , editing_cat = Nothing
          }
        , get_data
        )


{-|
        understand/
In addition to user interaction and HTTP events, we can "subscribe" to
push events - timers and websockets.

        outcome/
We need to keep abreast of the time so we keep updating ourselves every
second.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.second OnTick



{--
        understand/
In Elm any communication with JavaScript goes through a port. This is
like a hole in the side of the Elm program where values go in and out.
These work exactly like the commands and subscriptions
    - Sending values out to JS is a command
    - Listening for values coming in from JS is a subscription
-}


{-|
        understand/
In order to show/hide our window we need to inform the electron main
process to do it (elm can only mange a 'div-window' that we have created
ourselves). Therefore we create a port through which we send a message
asking the main process to show/hide our browser window.
-}
port show : String -> Cmd msg



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
    | OnTick Time
    | ToggleSelect Int
    | SelectFirstPing
    | AddCurrentTag String
    | RemoveCurrentTag String
    | EditCat String
    | EditUnCat
    | EditCat_TagCat String String
    | EditCat_NewTag String
    | EditCat_AddTag
    | EditCatDone
    | EditCatCancel
    | ServerGotCat String (Result Http.Error ())
    | ServerGotPing Int (Result Http.Error ())
    | HideWindow
    | ShowTODO
    | HideTODO


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveNewSch r ->
            saveNewSch r model

        SaveNewCats r ->
            saveNewCats r model

        SaveNewPings r ->
            saveNewPings r model

        AfterLogin r ->
            afterLogin r model

        OnTick t ->
            onTick t model

        ToggleSelect unx ->
            toggleSelect unx model

        SelectFirstPing ->
            selectFirstPing model

        AddCurrentTag tag ->
            addCurrentTag tag model

        RemoveCurrentTag tag ->
            removeCurrentTag tag model

        EditUnCat ->
            editUnCat model

        EditCat n ->
            editCat n model

        EditCat_TagCat tag cat ->
            editCatUpdTagCat tag cat model

        EditCat_NewTag tag ->
            editCatNewTag tag model

        EditCat_AddTag ->
            editCatAddTag model

        EditCatDone ->
            editCatDone model

        EditCatCancel ->
            editCatCancel model

        ServerGotCat n r ->
            serverGotCat n r model

        ServerGotPing u r ->
            serverGotPing u r model

        HideWindow ->
            ( model, show "off" )

        ShowTODO ->
            ( { model | dialog = Just NotDone }, Cmd.none )

        HideTODO ->
            ( { model | dialog = Nothing }, Cmd.none )


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

        outcome/
We gather the uncategorized tags into a list.
-}
uncategorizedTags : Model -> List String
uncategorizedTags model =
    List.foldr
        (\ping accum ->
            List.foldr
                (\tag accum ->
                    let
                        cat =
                            getCatForTag model tag
                    in
                        case cat of
                            Nothing ->
                                addNew tag accum

                            Just _ ->
                                accum
                )
                accum
                ping.tags
        )
        []
        model.pings


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
Process the latest ping schedule returned by the server. On error we
mark that an error has taken place so that refreshServerData can try to
reload it again.
-}
saveNewSch : Result Http.Error (List Sch) -> Model -> ( Model, Cmd Msg )
saveNewSch r model =
    case r of
        Ok sch ->
            ( addLatestPings { model | sch = sch }, Cmd.none )

        Err err ->
            onHttpError err { model | get_sch_failed = True } (getSch model.consts)


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
        to_cat name icon s_o tags =
            { name = name
            , icon = icon
            , tags = tags
            , recent = []
            , alpha = []
            , sort_order = s_o
            , rename = ""
            , saved = OnServer
            }

        decodeCat =
            Decode.map4 to_cat
                (Decode.field "name" Decode.string)
                (Decode.field "icon" Decode.string)
                (Decode.field "sort_order" Decode.int)
                (Decode.field "tags" (Decode.list Decode.string))
    in
        Decode.list decodeCat


{-|
        outcome/
Process the latest category data returned by the server. On error we
mark that an error has taken place so that refreshServerData can try to
reload it again.
-}
saveNewCats : Result Http.Error (List Category) -> Model -> ( Model, Cmd Msg )
saveNewCats r model =
    case r of
        Ok cats ->
            let
                clean_cats =
                    List.map (cleanTags model) cats

                u =
                    sync_merge_cats_1 model.cats clean_cats

                m =
                    { model | cats = u }

                un_cat =
                    uncategorizedTags m
            in
                ( { m | uncategorized = un_cat }, Cmd.none )

        Err err ->
            onHttpError err { model | get_cats_failed = True } (getCats model.consts)


{-|
        problem/
The category can contain a LOT of tags making it hard to use

        way/
We first make sure there are no duplicates and then create two lists -
one of recent elements and selected pings and the other alphabetically
arranged so they can all be seen.
-}
cleanTags : Model -> Category -> Category
cleanTags model cat =
    let
        unique_tags =
            List.foldr addNew [] cat.tags
    in
        { cat
            | tags = unique_tags
            , recent = make_recent_list_1 model unique_tags
            , alpha = make_alphabetical_list_1 unique_tags
        }


{-|
        outcome/
If the current list is available, use that otherwise use the first 10
pings and return all tags that are in this set.
-}
make_recent_list_1 : Model -> List String -> List String
make_recent_list_1 model tags =
    let
        pings =
            if Set.isEmpty model.current then
                List.take 10 model.pings
            else
                List.map pingFor (Set.toList model.current)

        recent t =
            not <| List.isEmpty <| List.filter (\p -> member t p.tags) pings
    in
        List.filter recent tags


make_alphabetical_list_1 : List String -> List String
make_alphabetical_list_1 tags =
    List.sort tags


{-|
        outcome/
So we sort the lists by name, merge them together, then re-sort them by
the sort order.
-}
sync_merge_cats_1 : List Category -> List Category -> List Category
sync_merge_cats_1 mycats svrcats =
    let
        s_mycats =
            List.sortBy .name mycats

        s_svrcats =
            List.sortBy .name svrcats
    in
        List.sortBy .sort_order <|
            sync_merge (\c1 c2 -> compare c1.name c2.name) s_mycats s_svrcats


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
            Decode.map2 (\unix tags -> Ping unix tags OnServer)
                (Decode.field "unix" Decode.int)
                (Decode.field "tags" (Decode.list Decode.string))
    in
        Decode.list decodePing


{-|
        outcome/
Process the latest ping data returned by the server. On error we mark
that an error has taken place so that refreshServerData can try to
reload it again.
-}
saveNewPings : Result Http.Error (List Ping) -> Model -> ( Model, Cmd Msg )
saveNewPings r model =
    case r of
        Ok pings ->
            let
                u =
                    sync_merge_pings_1 model.pings pings

                m =
                    { model | pings = u }

                un_cat =
                    uncategorizedTags m
            in
                ( { m | uncategorized = un_cat }, Cmd.none )

        Err err ->
            onHttpError err { model | get_pings_failed = True } (getPings model.consts)


{-|
        outcome/
So we sort the lists by name, merge them together, then re-sort them by
the sort order.
-}
sync_merge_pings_1 : List Ping -> List Ping -> List Ping
sync_merge_pings_1 mypings svrpings =
    let
        s_mypings =
            List.reverse mypings

        s_svrpings =
            List.reverse svrpings
    in
        List.reverse <|
            sync_merge (\p1 p2 -> compare p1.unix p2.unix) s_mypings s_svrpings



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
                loginThen_1 cmd model
            else
                ( Debug.log (toString err) model, Cmd.none )

        _ ->
            ( Debug.log (toString err) model, Cmd.none )


loginThen_1 : Cmd Msg -> Model -> ( Model, Cmd Msg )
loginThen_1 msg model =
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


{-|
        outcome/
We update our current time and perform time-based events:
    1. Update the ping list with the latest scheduled pings
    2. Request the latest data from the server
    3. If any new pings have been added, ensure that the window is shown
-}
onTick : Time -> Model -> ( Model, Cmd Msg )
onTick time model =
    let
        ( m, cmds ) =
            syncWithServer <|
                addLatestPings { model | last_tick = time }

        show_ping_window =
            if List.length m.pings == List.length model.pings then
                Cmd.none
            else
                show "on"
    in
        ( m, Cmd.batch (show_ping_window :: cmds) )


{-|
        problem/
We have a ping schedule with pings stretching into the future. However,
the user should have no clue as to when the next ping is coming from.

        way/
If there are no use pings, we only add the latest ping otherwise we
merge the user-entered pings with past pings so the user can fill any
missing ones.
-}
addLatestPings : Model -> Model
addLatestPings model =
    if List.isEmpty model.pings then
        add_one_ping_1 model
    else
        merge_sch_and_pings_1 model


add_one_ping_1 : Model -> Model
add_one_ping_1 model =
    case List.head model.sch of
        Nothing ->
            model

        Just unx ->
            { model | pings = [ pingFor unx ] }


merge_sch_and_pings_1 : Model -> Model
merge_sch_and_pings_1 model =
    let
        oldest_ping_time =
            case List.head (List.reverse model.pings) of
                Nothing ->
                    0

                Just p ->
                    p.unix

        past_ping_times =
            List.filter
                (\unx ->
                    let
                        t =
                            unix_to_tick unx
                    in
                        t <= model.last_tick && unx > oldest_ping_time
                )
                model.sch

        u =
            merge_1 past_ping_times model.pings
    in
        { model | pings = u }


merge_1 : List Sch -> List Ping -> List Ping
merge_1 unx pings =
    case ( unx, pings ) of
        ( [], [] ) ->
            []

        ( _, [] ) ->
            List.map pingFor unx

        ( [], _ ) ->
            pings

        ( h_u :: t_u, h_p :: t_p ) ->
            if h_u > h_p.unix then
                pingFor h_u :: merge_1 t_u pings
            else if h_u == h_p.unix then
                h_p :: merge_1 t_u t_p
            else
                h_p :: merge_1 unx t_p


{-|
        outcome/
Send any updates we have and after that, get the latest updates from the
server.
-}
syncWithServer : Model -> ( Model, List (Cmd Msg) )
syncWithServer model =
    let
        no_updates =
            no_cat_updates && no_ping_updates

        no_cat_updates =
            List.isEmpty (List.filter modified model.cats)

        no_ping_updates =
            List.isEmpty (List.filter modified model.pings)

        modified a =
            case a.saved of
                OnServer ->
                    False

                InMemory ->
                    True

                InFlight ->
                    True

                ServerSaysNo ->
                    False
    in
        if no_updates then
            refreshServerData model
        else
            sendUpdates model


{-|
        problem/
We need to get the latest data from the server because we may have
updated our data across multiple devices.

        way/
We can keep fetching data from the server.  However, we cannot blindly
request data every millisecond as that would eat up our resources and be
very badly behaved.

We could ask at a reasonable interval - say every 5 minutes - and that
would be almost perfect. However, it would show stale data for pings
that came up within the interval period so if you entered data on your
iPad and switched to your computer in the 5 minutes it would not show
correctly.

So we actually care about showing the latest data before a new ping.
Since we care about this, we will make a call 30 seconds before every
ping and hope that will give us the fresh data back in time.

TODO: We should retrive schedule only a few times a week (not as often
as user sync data)
TODO: We should retry error fetches at increasing intervals (1sec, 5sec,
30sec, 2mins,...)
-}
refreshServerData : Model -> ( Model, List (Cmd Msg) )
refreshServerData model =
    let
        consts =
            model.consts

        get_data =
            [ getSch consts, getCats consts, getPings consts ]

        get_on_failure =
            List.map (\( _, cmd ) -> cmd) <|
                List.filter (\( failed, _ ) -> failed)
                    [ ( model.get_sch_failed, getSch consts )
                    , ( model.get_cats_failed, getCats consts )
                    , ( model.get_pings_failed, getPings consts )
                    ]

        m =
            { model
                | get_sch_failed = False
                , get_cats_failed = False
                , get_pings_failed = False
            }

        next_ping =
            List.foldl
                (\unx acc ->
                    if unix_to_tick unx >= model.last_tick then
                        Just unx
                    else
                        acc
                )
                Nothing
                model.sch

        ping_in_30_secs =
            case next_ping of
                Nothing ->
                    True

                Just unx ->
                    let
                        ms =
                            unix_to_tick unx - model.last_tick
                    in
                        ms >= 30000 && ms < 30999
    in
        if ping_in_30_secs then
            ( m, get_data )
        else
            ( m, get_on_failure )


{-|
        outcome/
Send category updates and ping updates
TODO: Batch updates?
-}
sendUpdates : Model -> ( Model, List (Cmd Msg) )
sendUpdates model =
    let
        updated a =
            case a.saved of
                InMemory ->
                    True

                _ ->
                    False

        cat_upd =
            List.filter updated model.cats

        ping_upd =
            List.filter updated model.pings

        cat_updates =
            List.map (sendCatUpdate model) cat_upd

        ping_updates =
            List.map (sendPingUpdate model) ping_upd

        updates =
            List.append cat_updates ping_updates

        in_flight a =
            case a.saved of
                InMemory ->
                    { a | saved = InFlight }

                _ ->
                    a

        cats_in_flight =
            List.map in_flight model.cats

        pings_in_flight =
            List.map in_flight model.pings
    in
        ( { model | cats = cats_in_flight, pings = pings_in_flight }, updates )



{--
        outcome/
Send category update request and handle response
-}


sendCatUpdate : Model -> Category -> Cmd Msg
sendCatUpdate model cat =
    let
        consts =
            model.consts

        url =
            apiURL consts "api/cat"

        body =
            Http.jsonBody (encodeCat cat)

        req =
            put url body
    in
        Http.send (ServerGotCat cat.name) req


encodeCat : Category -> Encode.Value
encodeCat cat =
    Encode.object
        [ ( "name", Encode.string cat.name )
        , ( "icon", Encode.string cat.icon )
        , ( "sort_order", Encode.int cat.sort_order )
        , ( "tags", Encode.list (List.map Encode.string cat.tags) )
        ]


serverGotCat : String -> Result Http.Error () -> Model -> ( Model, Cmd Msg )
serverGotCat name r model =
    let
        saved =
            updSavedStatus ("Category " ++ name) r

        cats =
            List.map
                (\cat ->
                    if strEq cat.name name then
                        case cat.saved of
                            InFlight ->
                                { cat | saved = saved }

                            _ ->
                                cat
                    else
                        cat
                )
                model.cats
    in
        ( { model | cats = cats }, Cmd.none )



{--
        outcome/
Send ping update request and handle response
-}


sendPingUpdate : Model -> Ping -> Cmd Msg
sendPingUpdate model ping =
    let
        consts =
            model.consts

        url =
            apiURL consts "api/ping"

        body =
            Http.jsonBody (encodePing ping)

        req =
            put url body
    in
        Http.send (ServerGotPing ping.unix) req


encodePing : Ping -> Encode.Value
encodePing ping =
    Encode.object
        [ ( "unix", Encode.int ping.unix )
        , ( "tags", Encode.list (List.map Encode.string ping.tags) )
        ]


serverGotPing : Int -> Result Http.Error () -> Model -> ( Model, Cmd Msg )
serverGotPing unix r model =
    let
        saved =
            updSavedStatus ("Ping " ++ toString (unix_to_local unix)) r

        pings =
            List.map
                (\ping ->
                    if ping.unix == unix then
                        case ping.saved of
                            InFlight ->
                                { ping | saved = saved }

                            _ ->
                                ping
                    else
                        ping
                )
                model.pings
    in
        ( { model | pings = pings }, Cmd.none )


{-|
        outcome/
Returns the appropriate saved status for InFlight objects based on the
Http response
-}
updSavedStatus : String -> Result Http.Error () -> SyncStatus
updSavedStatus obj result =
    case result of
        Ok _ ->
            OnServer

        Err err ->
            let
                msg =
                    "Saving " ++ obj ++ ": "
            in
                case err of
                    Http.BadUrl url ->
                        Debug.log (msg ++ "Unexpected Error: 434 " ++ url) ServerSaysNo

                    Http.Timeout ->
                        Debug.log (msg ++ "Timeout") InMemory

                    Http.NetworkError ->
                        Debug.log (msg ++ "Network error") InMemory

                    Http.BadStatus err ->
                        let
                            _ =
                                Debug.log (msg ++ "Server Refused to Save") err
                        in
                            ServerSaysNo

                    Http.BadPayload err res ->
                        let
                            _ =
                                Debug.log (msg ++ "Server sent bad response:" ++ err) res
                        in
                            InMemory


{-|
        outcome/
Toggle ping in/out current selection
-}
toggleSelect : Int -> Model -> ( Model, Cmd Msg )
toggleSelect unx model =
    let
        current =
            if Set.member unx model.current then
                Set.remove unx model.current
            else
                Set.insert unx model.current
    in
        ( { model | current = current }, Cmd.none )


{-|
        situtation/
There are no current selections and the user is trying to add a new tag.

        outcome/
We select the first/latest ping so that the user can add tags.
-}
selectFirstPing : Model -> ( Model, Cmd Msg )
selectFirstPing model =
    let
        first_ping =
            List.head model.pings
    in
        case first_ping of
            Nothing ->
                ( model, Cmd.none )

            Just p ->
                ( { model | current = Set.singleton p.unix }, Cmd.none )


{-|
        outcome/
Add the current tag to the current selections
-}
addCurrentTag : String -> Model -> ( Model, Cmd Msg )
addCurrentTag tag model =
    let
        add_tag_to_curr p =
            if Set.member p.unix model.current then
                if not (member tag p.tags) then
                    { p | tags = tag :: p.tags, saved = InMemory }
                else
                    p
            else
                p

        upd =
            List.map add_tag_to_curr model.pings
    in
        ( { model | pings = upd }, Cmd.none )


{-|
        outcome/
Remove the current tag from the current selection
-}
removeCurrentTag : String -> Model -> ( Model, Cmd Msg )
removeCurrentTag tag model =
    let
        rm_tag_from_curr p =
            if Set.member p.unix model.current then
                let
                    u =
                        List.filter (\t -> not <| strEq t tag) p.tags
                in
                    if List.length p.tags == List.length u then
                        p
                    else
                        { p | tags = u, saved = InMemory }
            else
                p

        upd =
            List.map rm_tag_from_curr model.pings
    in
        ( { model | pings = upd }, Cmd.none )


{-|
        outcome/
Set up the editing category data and launch the editing category dialog
-}
editCat : String -> Model -> ( Model, Cmd Msg )
editCat name model =
    let
        cat =
            getCat model name
    in
        case cat of
            Nothing ->
                ( Debug.log "Unexpected Error: 732" model, Cmd.none )

            Just c ->
                ( { model
                    | dialog = Just CatEdit
                    , editing_cat = Just (editingCatFrom model c)
                  }
                , Cmd.none
                )


{-|
        outcome/
Update the tag's category in the category editing dialog
-}
editCatUpdTagCat : String -> String -> Model -> ( Model, Cmd Msg )
editCatUpdTagCat tag cat model =
    case model.editing_cat of
        Nothing ->
            ( Debug.log "Unexpected Error: 533" model, Cmd.none )

        Just editing ->
            let
                tags =
                    List.map
                        (\ct ->
                            if strEq ct.name tag then
                                { ct | cat = cat }
                            else
                                ct
                        )
                        editing.tags

                u =
                    { editing | tags = tags }
            in
                ( { model | editing_cat = Just u }, Cmd.none )


{-|
        outcome/
Update the new tag
-}
editCatNewTag : String -> Model -> ( Model, Cmd Msg )
editCatNewTag tag model =
    case model.editing_cat of
        Nothing ->
            ( Debug.log "Unexpected Error: 534" model, Cmd.none )

        Just editing ->
            let
                u =
                    { editing | newtag = tag }
            in
                ( { model | editing_cat = Just u }, Cmd.none )


{-|
        outcome/
Add a new tag to the current editing category
-}
editCatAddTag : Model -> ( Model, Cmd Msg )
editCatAddTag model =
    case model.editing_cat of
        Nothing ->
            ( Debug.log "Unexpected Error: 535" model, Cmd.none )

        Just editing ->
            let
                tag =
                    String.trim editing.newtag

                ct =
                    CatTag tag "" (editing.name)

                tags =
                    ct :: editing.tags

                u =
                    { editing | newtag = "", tags = tags }
            in
                if tag == "" then
                    ( model, Cmd.none )
                else
                    ( { model | editing_cat = Just u }, Cmd.none )


{-|
        outcome/
Edit all the uncategorized tags
-}
editUnCat : Model -> ( Model, Cmd Msg )
editUnCat model =
    let
        cat =
            { name = uncategorized_name
            , icon = "/r/noun_101469_cc.png"
            , tags = model.uncategorized
            , recent = []
            , alpha = []
            , sort_order = 0
            , rename = ""
            , saved = InMemory
            }
    in
        ( { model
            | dialog = Just CatEdit
            , editing_cat = Just (editingCatFrom model cat)
          }
        , Cmd.none
        )


{-|
        outcome/
Category editing completed so we convert the category editing data back
into the model data and dismiss the dialog and data.
-}
editCatDone : Model -> ( Model, Cmd Msg )
editCatDone model =
    let
        m =
            { model | dialog = Nothing, editing_cat = Nothing }
    in
        case model.editing_cat of
            Nothing ->
                let
                    err =
                        Debug.log "Unexpected Error: 663" model
                in
                    ( m, Cmd.none )

            Just edited ->
                let
                    ( cats, renames ) =
                        fromEditingCat model edited

                    m2 =
                        { m | cats = cats }

                    un_cat =
                        uncategorizedTags m2
                in
                    {--TODO: rename --}
                    ( { m2 | uncategorized = un_cat }, Cmd.none )


{-|
        outcome/
Category editing discarded so simply dismiss the dialog and data
-}
editCatCancel : Model -> ( Model, Cmd Msg )
editCatCancel model =
    ( { model | dialog = Nothing, editing_cat = Nothing }, Cmd.none )



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
        , top_bar
        , settings_btn
        , close_btn
        , ping_list model
        , title model
        , category_pane model
        , bottom_bar
        , dialog_box model
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
                , ( "height", px friction_bar_height )
                ]
    in
        Html.div [ style, HA.class "tt-dragable" ] []


settings_btn : Html.Html Msg
settings_btn =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px settings_btn_top )
                , ( "right", px settings_btn_right )
                , ( "width", px settings_btn_width )
                , ( "height", px settings_btn_height )
                , ( "background-image", bg settings_btn_icon )
                , ( "background-repeat", "no-repeat" )
                , ( "background-position", "center" )
                , ( "opacity", "0.8" )
                , ( "cursor", "pointer" )
                ]
    in
        Html.div [ style, HE.onClick ShowTODO ] []


close_btn : Html.Html Msg
close_btn =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px close_btn_top )
                , ( "right", px close_btn_right )
                , ( "width", px close_btn_width )
                , ( "height", px close_btn_height )
                , ( "background-image", bg close_btn_icon )
                , ( "background-repeat", "no-repeat" )
                , ( "background-position", "center" )
                , ( "cursor", "pointer" )
                ]
    in
        Html.div [ style, HE.onClick HideWindow ] []


top_bar : Html.Html Msg
top_bar =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", "0" )
                , ( "width", "100%" )
                , ( "height", px top_bar_height )
                , ( "border-bottom", "1px solid rgb(152, 152, 152)" )
                , ( "box-shadow", "rgb(200,200,200) 0px 1px 0px" )
                ]
    in
        Html.div [ style ] []


ping_list : Model -> Html.Html Msg
ping_list model =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px ping_panel_top )
                , ( "right", "0" )
                , ( "height", px ping_panel_height )
                , ( "width", px ping_panel_width )
                , ( "overflow", "scroll" )
                , ( "font-size", "14px" )
                , ( "cursor", "pointer" )
                ]

        ndx =
            List.range 1 (List.length model.pings)
    in
        Html.div [ style ]
            [ Html.table
                [ HA.attribute "cellspacing" "0"
                , HA.attribute "cellpadding" "0"
                ]
                (List.map2 (show_ping_1 model) ndx model.pings)
            ]


show_ping_1 : Model -> Int -> Ping -> Html.Html Msg
show_ping_1 model ndx ping =
    let
        style_v =
            if Set.member ping.unix model.current then
                [ ( "background", "#277ec1" )
                , ( "box-shadow", "inset 0px -1px 1px #fffdfd" )
                ]
            else
                [ ( "background", "linear-gradient(#d0d0d0, #d9d9d9, #e9e9e9)" ) ]

        style =
            HA.style <|
                List.append style_v
                    [ ( "height", px ping_entry_height ) ]
    in
        Html.tr
            [ style
            , HE.onClick (ToggleSelect ping.unix)
            ]
            [ ping_when_1 ping, ping_tags_1 ping ]


ping_when_1 : Ping -> Html.Html Msg
ping_when_1 ping =
    let
        style =
            HA.style
                [ ( "width", px ping_time_width )
                , ( "padding-left", "8px" )
                ]
    in
        Html.td [ style ] [ Html.text <| hhmm ping.unix ]


ping_tags_1 : Ping -> Html.Html Msg
ping_tags_1 ping =
    let
        tags =
            String.join ", " ping.tags

        style =
            HA.style
                [ ( "width", "100%" )
                , ( "padding", "8px" )
                ]
    in
        Html.td [ style ] [ Html.text tags ]


title : Model -> Html.Html Msg
title model =
    let
        cnt =
            if Set.isEmpty model.current then
                show_tagtime_title_1
            else
                show_current_title_1 model

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "left", px title_left )
                , ( "top", px title_top )
                , ( "width", px title_width )
                , ( "height", px title_height )
                , ( "text-align", "center" )
                , ( "font-family", "Merriweather" )
                ]
    in
        Html.div [ style ]
            [ cnt
            , subtitle_1
            ]


show_tagtime_title_1 : Html.Html Msg
show_tagtime_title_1 =
    let
        style =
            HA.style
                [ ( "font-size", "40px" )
                ]
    in
        Html.div [ style ]
            [ Html.text "Tag Time" ]


show_current_title_1 : Model -> Html.Html Msg
show_current_title_1 model =
    let
        l =
            Set.toList model.current
    in
        case List.head l of
            Nothing ->
                show_tagtime_title_1

            Just unx_min ->
                case List.head (List.reverse l) of
                    Nothing ->
                        show_tagtime_title_1

                    Just unx_max ->
                        show_ping_title_1 unx_min unx_max


show_ping_title_1 : Int -> Int -> Html.Html Msg
show_ping_title_1 unx_min unx_max =
    let
        cal_style =
            HA.style
                [ ( "font-size", "40px" )
                , ( "margin-bottom", "10px" )
                ]

        hhmm_style =
            HA.style [ ( "font-size", "24px" ) ]

        range_style =
            HA.style
                [ ( "font-size", "16px" )
                , ( "font-style", "italic" )
                ]

        hhmm_msg =
            if unx_min == unx_max then
                Html.div [ hhmm_style ] [ Html.text <| hhmm unx_max ]
            else if is_same_day unx_min unx_max then
                Html.div [ hhmm_style ]
                    [ Html.span [] [ Html.text <| hhmm unx_max ]
                    , Html.span [ range_style ]
                        [ Html.text (" - " ++ hhmm unx_min) ]
                    ]
            else
                Html.div [ hhmm_style ]
                    [ Html.span [ range_style ] [ Html.text <| hhmm unx_max ]
                    , Html.span [ range_style ]
                        [ Html.text (" - " ++ cal unx_min ++ " " ++ hhmm unx_min) ]
                    ]
    in
        Html.div []
            [ Html.div [ cal_style ] [ Html.text <| cal unx_max ]
            , hhmm_msg
            ]


subtitle_1 : Html.Html Msg
subtitle_1 =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "bottom", "8px" )
                , ( "width", px title_width )
                , ( "text-align", "center" )
                , ( "font-size", "14px" )
                , ( "font-style", "italic" )
                ]
    in
        Html.div [ style ]
            [ Html.text "What are you doing right now?" ]


category_pane : Model -> Html.Html Msg
category_pane model =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "left", "0" )
                , ( "top", px category_pane_top )
                , ( "width", px category_pane_width )
                , ( "height", px category_pane_height )
                , ( "background", "white" )
                , ( "border-right", "1px solid #f3f2f2" )
                , ( "border-left", "2px solid #b3b3b3" )
                , ( "box-shadow", "inset -1px 0px 2px 0px #cac9c9" )
                , ( "overflow", "scroll" )
                ]

        blocks =
            List.reverse <|
                uncategorized_tags_1 model
                    :: List.reverse (categorized_tags_1 model)
    in
        Html.div [ style ] blocks


categorized_tags_1 : Model -> List (Html.Html Msg)
categorized_tags_1 model =
    let
        color_map =
            prop_list category_card_colors (List.length model.cats)

        top_map =
            prop_list category_card_top_pos (List.length model.cats)

        ndx =
            List.range 1 (List.length model.cats)
    in
        (List.map4 (category_1 model) ndx color_map top_map model.cats)


category_1 : Model -> Int -> ( String, String, String ) -> Int -> Category -> Html.Html Msg
category_1 model ndx color top cat =
    let
        width =
            category_pane_width // (List.length model.cats) - category_card_gap

        left =
            (width + category_card_gap) * (ndx - 1) + category_card_gap // 2

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "width", px width )
                , ( "top", px top )
                , ( "left", px left )
                , ( "border-radius", px category_card_border_radius )
                , ( "overflow", "hidden" )
                , ( "text-align", "center" )
                , ( "box-shadow", "1px 2px 1px #b7b7b7" )
                ]

        ( head_color, sel_color, inset_color ) =
            color
    in
        Html.div [ style ]
            [ category_head_1 model width head_color cat
            , category_tags_1 model width sel_color inset_color cat
            ]


category_head_1 : Model -> Int -> String -> Category -> Html.Html Msg
category_head_1 model width color cat =
    let
        icon =
            imgURL model.consts cat.icon

        settings_style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px cat_settings_top )
                , ( "right", px cat_settings_right )
                , ( "width", px cat_settings_width )
                , ( "opacity", "0.7" )
                ]

        iconstyle =
            HA.style
                [ ( "width", px cat_icon_sz )
                , ( "height", px cat_icon_sz )
                ]

        titlestyle =
            HA.style
                [ ( "font-size", "16px" )
                ]

        style =
            HA.style
                [ ( "width", px width )
                , ( "background", color )
                , ( "padding", "12px 0" )
                , ( "cursor", "pointer" )
                ]
    in
        Html.div [ style, HE.onClick (EditCat cat.name) ]
            [ Html.img [ iconstyle, HA.src icon ] []
            , Html.div [ titlestyle ] [ Html.text (currName cat) ]
            , Html.img
                [ class "cat-settings"
                , settings_style
                , HA.src cat_settings_icon
                ]
                []
            ]


category_tags_1 : Model -> Int -> String -> String -> Category -> Html.Html Msg
category_tags_1 model width color inset_color cat =
    let
        style =
            HA.style
                [ ( "width", px width )
                , ( "background", "linear-gradient(to bottom right, #efefef, #fff3d9)" )
                ]
    in
        Html.div [ style ]
            (List.map (show_category_tag_1 model color inset_color) cat.tags)


show_category_tag_1 : Model -> String -> String -> String -> Html.Html Msg
show_category_tag_1 model color inset_color tag =
    let
        style =
            HA.style
                [ ( "font-size", "12px" )
                , ( "text-align", "center" )
                , ( "height", px category_tag_height )
                , ( "line-height", px category_tag_height )
                , ( "cursor", "pointer" )
                ]

        sel_style =
            [ ( "display", "inline-block" )
            , ( "min-width", "72px" )
            , ( "padding", "0 8px" )
            , ( "height", "28px" )
            , ( "line-height", "28px" )
            , ( "border-radius", "3px" )
            , ( "background", color )
            , ( "box-shadow", "inset 1px 1px " ++ inset_color )
            ]

        selstatus =
            tagInSelection model tag

        ( btn_style, msg ) =
            case selstatus of
                InAll ->
                    ( HA.style sel_style, RemoveCurrentTag tag )

                InSome ->
                    ( HA.style <| ( "opacity", "0.6" ) :: sel_style
                    , AddCurrentTag tag
                    )

                InNone ->
                    ( HA.style [], AddCurrentTag tag )

                NoSelection ->
                    ( HA.style [], SelectFirstPing )
    in
        Html.div [ style, HE.onClick msg ]
            [ Html.div [ btn_style ] [ Html.text tag ] ]


uncategorized_tags_1 : Model -> Html.Html Msg
uncategorized_tags_1 model =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "bottom", px uncategorized_strip_bottom )
                , ( "left", px uncategorized_strip_left )
                , ( "width", px uncategorized_strip_width )
                , ( "height", px uncategorized_strip_height )
                , ( "background", uncategorized_strip_color )
                , ( "box-shadow", "inset 0px 1px 1px #9a9a9a" )
                , ( "font-size", "12px" )
                , ( "cursor", "pointer" )
                ]
    in
        Html.div [ style ]
            [ uncat_tags_1 model
            , uncat_title_1
            ]


uncat_title_1 : Html.Html Msg
uncat_title_1 =
    let
        style =
            HA.style
                [ ( "width", px uncategorized_strip_title_width )
                , ( "height", px uncategorized_strip_height )
                , ( "line-height", px uncategorized_strip_height )
                , ( "box-shadow", "1px 1px 0.5px #525252" )
                , ( "text-align", "center" )
                , ( "background", uncategorized_card_color )
                ]

        icon_style =
            HA.style
                [ ( "opacity", "0.75" )
                , ( "margin-bottom", "-4px" )
                ]

        txt_style =
            HA.style [ ( "padding-left", "0.5em" ) ]
    in
        Html.div [ style, HE.onClick EditUnCat ]
            [ Html.img [ icon_style, HA.src cat_settings_icon ] []
            , Html.span [ txt_style ] [ Html.text "Uncategorized:" ]
            ]


uncat_tags_1 : Model -> Html.Html Msg
uncat_tags_1 model =
    let
        width =
            uncategorized_strip_width - uncategorized_strip_title_width

        style =
            HA.style
                [ ( "width", px width )
                , ( "overflow", "scroll" )
                , ( "float", "right" )
                , ( "line-height", px uncategorized_strip_height )
                ]
    in
        Html.div [ style ]
            (List.map (uncat_tag_1 model) model.uncategorized)


uncat_tag_1 : Model -> String -> Html.Html Msg
uncat_tag_1 model tag =
    let
        style =
            [ ( "font-style", "italic" )
            , ( "padding", "2px 4px" )
            , ( "margin", "0 2px" )
            , ( "cursor", "pointer" )
            ]

        all_sel_style =
            List.append style
                [ ( "background", "#d0d0d0" )
                , ( "border", "1px ridge #929292" )
                , ( "border-radius", "4px" )
                ]

        some_sel_style =
            ( "opacity", "0.6" ) :: all_sel_style

        selstatus =
            tagInSelection model tag

        ( btn_style, msg ) =
            case selstatus of
                InAll ->
                    ( HA.style all_sel_style, RemoveCurrentTag tag )

                InSome ->
                    ( HA.style some_sel_style, AddCurrentTag tag )

                InNone ->
                    ( HA.style style, AddCurrentTag tag )

                NoSelection ->
                    ( HA.style style, SelectFirstPing )
    in
        Html.span [ btn_style, HE.onClick msg ] [ Html.text tag ]


bottom_bar : Html.Html Msg
bottom_bar =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "bottom", "0" )
                , ( "width", "100%" )
                , ( "height", px bottom_bar_height )
                , ( "border-top", "1px solid #f0f0f0" )
                ]
    in
        Html.div [ style ] [ help_txt_1 ]


help_txt_1 : Html.Html Msg
help_txt_1 =
    let
        style =
            HA.style
                [ ( "float", "right" )
                , ( "margin-right", "2em" )
                , ( "margin-top", "2px" )
                , ( "font-size", "12px" )
                , ( "opacity", "0.8" )
                ]
    in
        Html.a [ style, HA.href "#", HE.onClick ShowTODO ] [ Html.text "(?) Help/Support" ]


dialog_box : Model -> Html.Html Msg
dialog_box model =
    let
        pos =
            dialog_top_pos_1 model
    in
        Html.div []
            [ overlay_1 pos.overlay_top_pos
            , not_done_dialog_1 pos.not_done_top_pos
            , cat_edit_dialog_1 model pos.cat_edit_top_pos
            ]


type alias DialogPos =
    { overlay_top_pos : Int
    , not_done_top_pos : Int
    , cat_edit_top_pos : Int
    }


{-|
        situation/
We need to 'slide-in' dialogs so they animate in nicely.

        outcome/
For this, we create the dialogs "offscreen" and then bring them in (with
a CSS transistion) whenever they need to be shown. We set the position
based on which dialog is being shown.
-}
dialog_top_pos_1 : Model -> DialogPos
dialog_top_pos_1 model =
    let
        out_of_sight =
            1000

        overlay_top_pos =
            0

        not_done_top_pos =
            (window_height - dialog_not_done_height) // 2

        cat_edit_top_pos =
            (window_height - cat_edit_height) // 2
    in
        case model.dialog of
            Nothing ->
                { overlay_top_pos = out_of_sight
                , not_done_top_pos = out_of_sight
                , cat_edit_top_pos = out_of_sight
                }

            Just d ->
                case d of
                    NotDone ->
                        { overlay_top_pos = overlay_top_pos
                        , not_done_top_pos = not_done_top_pos
                        , cat_edit_top_pos = out_of_sight
                        }

                    CatEdit ->
                        { overlay_top_pos = overlay_top_pos
                        , not_done_top_pos = out_of_sight
                        , cat_edit_top_pos = cat_edit_top_pos
                        }


overlay_1 : Int -> Html.Html Msg
overlay_1 top_pos =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "left", "0" )
                , ( "top", px top_pos )
                , ( "width", px window_width )
                , ( "height", px window_height )
                , ( "background", "black" )
                , ( "opacity", "0.7" )
                ]
    in
        Html.div [ style ] []


not_done_dialog_1 : Int -> Html.Html Msg
not_done_dialog_1 top_pos =
    let
        not_done_left_pos =
            (window_width - dialog_not_done_width) // 2

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px top_pos )
                , ( "left", px not_done_left_pos )
                , ( "width", px dialog_not_done_width )
                , ( "height", px dialog_not_done_height )
                , ( "background-color", "white" )
                , ( "border-radius", "8px" )
                , ( "border", "1px solid black" )
                , ( "transition", "top 250ms" )
                , ( "text-align", "center" )
                , ( "padding-top", "1em" )
                ]

        ok_btn =
            Html.div
                [ HA.style [ ( "margin-top", "2em" ) ]
                , class "button"
                , HE.onClick HideTODO
                ]
                [ Html.text "Ok" ]
    in
        Html.div [ style ] [ Html.text "(To be implemented...)", ok_btn ]


cat_edit_dialog_1 : Model -> Int -> Html.Html Msg
cat_edit_dialog_1 model top_pos =
    let
        err_icon =
            imgURL model.consts "cat-error.png"

        editing =
            case model.editing_cat of
                Nothing ->
                    EditingCat "(NO CATEGORY SELECTED)" "" err_icon [] ""

                Just c ->
                    c

        color_map =
            prop_list category_card_colors (List.length model.cats)

        color =
            List.foldr
                (\( cat, clr ) acc ->
                    if strEq cat.name editing.name then
                        clr
                    else
                        acc
                )
                uncategorized_card_color
                (List.map2 (\cat ( hdr, _, _ ) -> ( cat, hdr )) model.cats color_map)

        cat_edit_left_pos =
            (window_width - cat_edit_width) // 2

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px top_pos )
                , ( "left", px cat_edit_left_pos )
                , ( "width", px cat_edit_width )
                , ( "height", px cat_edit_height )
                , ( "border-radius", "8px" )
                , ( "overflow", "hidden" )
                , ( "transition", "top 250ms" )
                , ( "box-shadow", "2px 2px 1px #212121" )
                ]
    in
        Html.div [ style ]
            [ cat_edit_head_1 model editing color
            , cat_edit_tags_1 model editing
            ]


cat_edit_head_1 : Model -> EditingCat -> String -> Html.Html Msg
cat_edit_head_1 model editing color =
    let
        icon =
            imgURL model.consts editing.icon

        iconstyle =
            HA.style
                [ ( "width", "48px" )
                , ( "height", "48px" )
                ]

        titlestyle =
            HA.style
                [ ( "font-size", "16px" )
                ]

        style =
            HA.style
                [ ( "width", px cat_edit_head_width )
                , ( "height", px cat_edit_head_height )
                , ( "background", color )
                ]

        inner_style =
            HA.style
                [ ( "text-align", "center" )
                , ( "padding-top", "5em" )
                ]

        add_inp_style =
            HA.style
                [ ( "width", "64px" )
                , ( "border", "0" )
                , ( "border-radius", "2px" )
                , ( "text-align", "center" )
                , ( "display", "block" )
                , ( "padding", "3px" )
                , ( "margin", "8px auto" )
                , ( "box-shadow", "inset 1px 1px 1px #404040" )
                ]

        add_inp =
            Html.input
                [ add_inp_style
                , HA.placeholder "New Tag"
                , HA.value editing.newtag
                , HE.onInput EditCat_NewTag
                ]
                []

        add_btn =
            Html.div
                [ class "button", HE.onClick EditCat_AddTag ]
                [ Html.text "New Tag" ]

        add_grp_style =
            HA.style [ ( "margin-top", "4em" ) ]

        add_grp =
            Html.div [ add_grp_style ] [ add_inp, add_btn ]
    in
        Html.div [ style ]
            [ Html.div [ inner_style ]
                [ Html.img [ iconstyle, HA.src icon ] []
                , Html.div [ titlestyle ] [ Html.text (currName editing) ]
                , add_grp
                ]
            ]


cat_edit_tags_1 : Model -> EditingCat -> Html.Html Msg
cat_edit_tags_1 model editing =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "left", px cat_edit_head_width )
                , ( "top", "0" )
                , ( "width", px (cat_edit_width - cat_edit_head_width) )
                , ( "height", px cat_edit_head_height )
                , ( "background", "linear-gradient(to bottom,white,#eaeaea)" )
                , ( "box-shadow", "inset 1px 0px 0px #eaeaea" )
                ]

        bottom =
            -8

        padding =
            32

        title_style =
            HA.style
                [ ( "font-size", "24px" )
                , ( "text-align", "center" )
                , ( "margin", "14px" )
                , ( "font-weight", "100" )
                ]

        btn_cnt_style =
            HA.style
                [ ( "position", "absolute" )
                , ( "bottom", px bottom )
                , ( "right", px padding )
                ]

        done_btn =
            Html.div
                [ class "button", HE.onClick EditCatDone ]
                [ Html.text "Done" ]

        cancel_btn =
            Html.div
                [ class "button", HE.onClick EditCatCancel ]
                [ Html.text "Cancel" ]
    in
        Html.div [ style ]
            [ Html.div [ title_style ] [ Html.text "Edit Tags" ]
            , Html.div [ btn_cnt_style ] [ done_btn, cancel_btn ]
            , cat_edit_tag_data_1 model editing
            ]


cat_edit_tag_data_1 : Model -> EditingCat -> Html.Html Msg
cat_edit_tag_data_1 model editing =
    let
        margin =
            32

        title_height_estimate =
            100

        height =
            cat_edit_head_height - 2 * margin - title_height_estimate

        style =
            HA.style
                [ ( "margin", px margin )
                , ( "height", px height )
                , ( "overflow", "scroll" )
                ]
    in
        Html.div [ style ] (List.map (cat_edit_tag_1 model) editing.tags)


cat_edit_tag_1 : Model -> CatTag -> Html.Html Msg
cat_edit_tag_1 model tag =
    let
        style =
            HA.style
                [ ( "display", "inline-block" )
                , ( "padding", "24px" )
                ]

        input_style =
            HA.style
                [ ( "width", "64px" )
                , ( "margin-right", "8px" )
                ]

        select_style =
            HA.style [ ( "width", "96px" ) ]

        cat_names =
            "" :: List.map currName model.cats

        select_options =
            List.map
                (\o ->
                    Html.option
                        [ HA.selected (strEq o tag.cat), HA.value o ]
                        [ Html.text o ]
                )
                cat_names
    in
        Html.div [ style ]
            [ Html.input [ input_style, HA.value (currName tag) ] []
            , Html.select
                [ select_style, HE.onInput (EditCat_TagCat tag.name) ]
                select_options
            ]



{--
        understand/
View component dimensions
-}


window_height : Int
window_height =
    600


window_width : Int
window_width =
    970


friction_bar_height : Int
friction_bar_height =
    64


top_bar_height : Int
top_bar_height =
    148


bottom_bar_height : Int
bottom_bar_height =
    24


ping_panel_top : Int
ping_panel_top =
    top_bar_height + 1


ping_panel_height : Int
ping_panel_height =
    window_height - top_bar_height - bottom_bar_height - 2


ping_panel_width : Int
ping_panel_width =
    256


ping_entry_height : Int
ping_entry_height =
    48


ping_time_width : Int
ping_time_width =
    64


category_pane_top : Int
category_pane_top =
    ping_panel_top


category_pane_width : Int
category_pane_width =
    window_width - ping_panel_width


category_pane_height : Int
category_pane_height =
    ping_panel_height


{-|
        understand/
Contains a tuple of: "Header Color", "Btn Color", "Btn Inset Color"
-}
category_card_colors : List ( String, String, String )
category_card_colors =
    [ ( "linear-gradient(to bottom, #85c720, #6ea718)"
      , "linear-gradient(to top, #85c720, #7ebf1c)"
      , "#679a18"
      )
    , ( "linear-gradient(to bottom, #f14050, #c52f3c)"
      , "linear-gradient(to top, #ff4556, #e23745)"
      , "#a72833"
      )
    , ( "linear-gradient(to bottom, #33dbdc,#21b2b3)"
      , "linear-gradient(to top, #33dbdc,#28d1d2)"
      , "#1e9c9c"
      )
    , ( "linear-gradient(to bottom, #ffb60a, #fb8f0a)"
      , "linear-gradient(to top, #ffca0a, #ffb60a)"
      , "#c78d02"
      )
    ]


category_card_top_pos : List Int
category_card_top_pos =
    [ 64, 128, 32, 72 ]


category_card_gap : Int
category_card_gap =
    24


category_card_border_radius : Int
category_card_border_radius =
    8


uncategorized_name : String
uncategorized_name =
    "(UNCATEGORIZED)"


uncategorized_card_color : String
uncategorized_card_color =
    "linear-gradient(170deg, #606060, #808080, #404040)"


uncategorized_strip_color : String
uncategorized_strip_color =
    "linear-gradient(#6f6f6f, #949393)"


uncategorized_strip_bottom : Int
uncategorized_strip_bottom =
    0


uncategorized_strip_left : Int
uncategorized_strip_left =
    1


uncategorized_strip_title_width : Int
uncategorized_strip_title_width =
    128


uncategorized_strip_width : Int
uncategorized_strip_width =
    category_pane_width - 2


uncategorized_strip_height : Int
uncategorized_strip_height =
    32


category_tag_height : Int
category_tag_height =
    48


settings_btn_icon : String
settings_btn_icon =
    "ping-settings.png"


settings_btn_top : Int
settings_btn_top =
    8


settings_btn_right : Int
settings_btn_right =
    close_btn_right + close_btn_width + 8


settings_btn_width : Int
settings_btn_width =
    21


settings_btn_height : Int
settings_btn_height =
    21


close_btn_icon : String
close_btn_icon =
    "ping-close-btn.png"


close_btn_top : Int
close_btn_top =
    8


close_btn_right : Int
close_btn_right =
    8


close_btn_width : Int
close_btn_width =
    21


close_btn_height : Int
close_btn_height =
    21


cat_icon_sz : Int
cat_icon_sz =
    48


cat_settings_icon : String
cat_settings_icon =
    "ping-cat-settings.png"


cat_settings_top : Int
cat_settings_top =
    8


cat_settings_right : Int
cat_settings_right =
    8


cat_settings_width : Int
cat_settings_width =
    16


dialog_not_done_height : Int
dialog_not_done_height =
    96


dialog_not_done_width : Int
dialog_not_done_width =
    256


cat_edit_height : Int
cat_edit_height =
    3 * window_height // 5


cat_edit_width : Int
cat_edit_width =
    4 * window_width // 5


cat_edit_head_height : Int
cat_edit_head_height =
    cat_edit_height


cat_edit_head_width : Int
cat_edit_head_width =
    round (toFloat (cat_edit_width) / golden_ratio) // 2


title_top : Int
title_top =
    24


title_left : Int
title_left =
    window_width // 3


title_width : Int
title_width =
    window_width // 3


title_height : Int
title_height =
    top_bar_height - title_top - 1



{--
        understand/
View helper functions
-}


golden_ratio : Float
golden_ratio =
    1.61803399


px : Int -> String
px v =
    (toString v) ++ "px"


bg : String -> String
bg i =
    "url(" ++ i ++ ")"


pad2 : Int -> String
pad2 v =
    if v < 10 then
        "0" ++ toString v
    else
        toString v


unix_to_local : Sch -> Date.Date
unix_to_local unix =
    Date.fromTime <| unix_to_tick unix


unix_to_tick : Sch -> Time.Time
unix_to_tick unix =
    toFloat <| unix * 1000


is_same_day : Int -> Int -> Bool
is_same_day unix1 unix2 =
    let
        date1 =
            unix_to_local unix1

        date2 =
            unix_to_local unix2
    in
        Date.year date1
            == Date.year date2
            && Date.month date1
            == Date.month date2
            && Date.day date1
            == Date.day date2


hhmm : Int -> String
hhmm unix =
    let
        date =
            unix_to_local unix
    in
        pad2 (Date.hour date)
            ++ ":"
            ++ pad2 (Date.minute date)


dow : Int -> String
dow unix =
    let
        date =
            unix_to_local unix
    in
        case Date.dayOfWeek date of
            Date.Mon ->
                "Mon"

            Date.Tue ->
                "Tue"

            Date.Wed ->
                "Wed"

            Date.Thu ->
                "Thu"

            Date.Fri ->
                "Fri"

            Date.Sat ->
                "Sat"

            Date.Sun ->
                "Sun"


month : Int -> String
month unix =
    let
        date =
            unix_to_local unix
    in
        case Date.month date of
            Date.Jan ->
                "Jan"

            Date.Feb ->
                "Feb"

            Date.Mar ->
                "Mar"

            Date.Apr ->
                "Apr"

            Date.May ->
                "May"

            Date.Jun ->
                "Jun"

            Date.Jul ->
                "Jul"

            Date.Aug ->
                "Aug"

            Date.Sep ->
                "Sep"

            Date.Oct ->
                "Oct"

            Date.Nov ->
                "Nov"

            Date.Dec ->
                "Dec"


day : Int -> String
day unix =
    let
        date =
            unix_to_local unix
    in
        pad2 (Date.day date)


cal : Int -> String
cal unix =
    let
        wd =
            dow unix

        mn =
            month unix

        dy =
            day unix
    in
        wd ++ ", " ++ dy ++ "/" ++ mn


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

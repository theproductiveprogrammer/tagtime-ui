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
import Task
import Dom
import Regex
import Dict
import Chart


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
Unix time is the the number of seconds that have elapsed since Thursday,
1 January 1970 UTC.
-}
type alias Unix =
    Int


{-|
        understand/
The ping schedule for the user (in unix time)
-}
type alias Sch =
    List Unix


{-|
        understand/
A "tag" represents a category of things being done at a given ping
-}
type alias Tag =
    String


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
    = NewObj
    | OnServer
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
    , tags : List Tag
    , sort_order : Int
    , saved : SyncStatus
    }


{-|
        problem/
When editing the tags we have to keep track of the following:
    1. The tag changed it's category
    2. The tag changed it's name
    3. The tag has been deleted
This information cannot be 'derived' from simple tag data.

        way/
We keep a separate data structure which fits our requirements more
naturally and convert back and forth from category to this data
structure.
-}
type alias EditingTag =
    { orig : Tag
    , name : String
    , cat : String
    , del : Bool
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
    , tags : List Tag
    , saved : SyncStatus
    }


{-|
        understand/
A 'Brick' represents an important task that has to be done today.
-}
type alias Brick =
    { task : String
    , tags : List Tag
    }


{-|
        understand/
List of dialog boxes to show
-}
type Dialog
    = NotDone
    | EditTags (List EditingTag)
    | EditBricks String


{-|
        understand/
The main model of TEA - The Elm Architecture. Everything happening in
our system (except for CSS animations) can be derived from this model so
it will have a hodge-podge of everything we need to keep track of.
-}
type alias Model =
    { -- deployment data --
      consts :
        Consts
        -- core data --
    , sch :
        Sch
    , cats :
        List Category
    , pings :
        List Ping
    , bricks :
        List Brick
        -- server request status --
    , get_sch_failed :
        Bool
    , get_cats_failed :
        Bool
    , get_pings_failed :
        Bool
        -- login req data --
    , after_login :
        List (Cmd Msg)
    , login_req_in_flight :
        Bool
        -- tick data --
    , last_tick :
        Time
        -- user action data --
    , current :
        Set Unix
        -- view helper data --
    , recent_tags :
        List Tag
    , all_tags_filter :
        String
    , all_tags :
        List Tag
        -- dialog status data --
    , dialog :
        Maybe Dialog
    }



{--
        understand/
Helper functions for dealing with the model data
-}


unix_to_local : Unix -> Date.Date
unix_to_local unix =
    Date.fromTime <| unix_to_tick unix


unix_to_tick : Unix -> Time.Time
unix_to_tick unix =
    toFloat <| unix * 1000


tick_to_unix : Time.Time -> Unix
tick_to_unix t =
    round <| t / 1000


is_same_day : Unix -> Unix -> Bool
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


pingFor : Int -> Ping
pingFor unix =
    Ping unix [] NewObj


getPing : Model -> Int -> Maybe Ping
getPing model unx =
    List.foldr
        (\ping acc ->
            if ping.unix == unx then
                Just ping
            else
                acc
        )
        Nothing
        model.pings


alwaysGetPing : Model -> Int -> Ping
alwaysGetPing model unx =
    case getPing model unx of
        Nothing ->
            pingFor unx

        Just p ->
            p


getCat : Model -> String -> Maybe Category
getCat model name =
    List.head <|
        List.filter (\c -> strEq c.name name) model.cats


getCatForTag : Model -> String -> Maybe Category
getCatForTag model tag =
    List.foldr
        (\cat acc ->
            if member cat.tags tag then
                Just cat
            else
                acc
        )
        Nothing
        model.cats


{-|
        outcome/
We keep a set of special tags (energy level, retro pings etc) and for
these we start them off with a "-". This means that no normal tags can
be allowed to start with "-".
-}
specialTag : Tag -> Bool
specialTag tag =
    String.startsWith "-" tag


{-|
        outcome/
Check if the given tag is in the current selection and return the status
-}
tagInSelection : Model -> String -> SelStatus
tagInSelection model tag =
    let
        sel_pings =
            Set.toList model.current |> List.map (alwaysGetPing model)

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
        outcome/
We clean the ping tags, the category tags, and cache the list of "all
available tags" for the user. This all helps when displaying in the view
so this is best thought off as a view helper function.
-}
cleanModelTags : Model -> Model
cleanModelTags model =
    let
        clean_cats =
            List.map (cleanTags model) model.cats

        clean_pings =
            List.map (cleanTags model) model.pings

        m =
            { model | pings = clean_pings, cats = clean_cats }

        all_tags =
            all_view_tags m
    in
        { m | all_tags = all_tags }


{-|
        outcome/
Return a complete list of all tags derived from the categories and the
pings (filtered by user seach string and sorted).  As this is for user
selection we ignore "special" pings like energy tags, sleep, retro, etc
-}
all_view_tags : Model -> List Tag
all_view_tags model =
    let
        ignore =
            specialTag

        get_tags elems acc =
            List.foldr
                (\e accum ->
                    List.foldr
                        (\tag ac ->
                            if member accum tag then
                                ac
                            else if ignore tag then
                                ac
                            else
                                tag :: ac
                        )
                        accum
                        e.tags
                )
                acc
                elems

        cat_tags =
            get_tags model.cats []

        tags =
            get_tags model.pings cat_tags

        all_tags =
            cleanTags model { tags = tags }
    in
        all_tags.tags


{-|
        outcome/
Apply any user filter and return the appropriate tags
-}
filtered_tags : Model -> List Tag
filtered_tags model =
    let
        normalize s =
            String.toLower <| String.trim s

        f =
            normalize model.all_tags_filter

        matches tag =
            if String.isEmpty f then
                True
            else
                String.contains f (normalize tag)
    in
        List.filter matches model.all_tags


{-|
        outcome/
This is the EditingTag version of `filtered_tags`
-}
filtered_edit_tags : Model -> List EditingTag -> List EditingTag
filtered_edit_tags model etags =
    let
        normalize s =
            String.toLower <| String.trim s

        f =
            normalize model.all_tags_filter

        matches etag =
            if String.isEmpty f then
                True
            else
                String.contains f (normalize etag.orig)
    in
        List.filter matches etags


{-|
        outcome/
We 'clean' the tag data by converting all equivalent tags to their
"canonical" values, remove duplicates, and put them in alphabetical
order.
-}
cleanTags : Model -> { a | tags : List Tag } -> { a | tags : List Tag }
cleanTags model elem =
    let
        empty tag =
            String.isEmpty <| String.trim tag

        clean orig =
            List.foldr
                (\t accum ->
                    if member accum t || empty t then
                        accum
                    else
                        get_canonical_tag model t :: accum
                )
                []
                orig

        order tags =
            List.sortWith case_insensitive_cmp tags
    in
        { elem | tags = elem.tags |> clean |> order }


{-|
        outcome/
We keep a tag list which can be used to speed up repeated entries from
the recent and selected pings.
-}
loadRecentTags : Model -> Model
loadRecentTags model =
    let
        recent_tags =
            get_recent_tags_1 model
    in
        { model | recent_tags = recent_tags }


{-|
        outcome/
Take all the selection and the most recent handful of pings and gather
all their tags in order.
-}
get_recent_tags_1 : Model -> List Tag
get_recent_tags_1 model =
    let
        accum_tags pings =
            List.foldl
                (\p accum ->
                    List.foldl
                        (\t acc ->
                            if member acc t then
                                acc
                            else
                                t :: acc
                        )
                        accum
                        p.tags
                )
                []
                pings

        ping_handful =
            List.append selected_pings handful_of_latest

        selected_pings =
            List.reverse (Set.toList model.current)
                |> List.map (alwaysGetPing model)

        handful_of_latest =
            List.take 20 model.pings
    in
        List.reverse <| accum_tags ping_handful


{-|
        problem/
Given a set of recent tags and a taglist we must put them together in a
way that allows users to easily select.

        way/
There are two main ways of making a list navigable:
    1. By recency
    2. By (alphabetical) order
The recency list is usually much faster for a small number of elements
but becomes confusing when there are more than a handful (let's use the
magic number ten). After that an alphabetical ordering is best.

Now the list given to us is expected to be in alphabetical order. All we
need to do is put a set of recent tags in front of it. If the recent
tags are too few or the remaining tags are too few then we should be
careful to remove duplicates from the alphabetical list. On the other
hand, if they are a lot then we should allow duplicates (the user will
'switch modes' from recent to alphabetical in his search).
-}
make_selectable_list : Model -> List Tag -> List Tag
make_selectable_list model tags =
    let
        the_magic_number =
            10

        recent_tags =
            List.filter (member tags) model.recent_tags

        small_recent_tags =
            List.take the_magic_number recent_tags

        dedup_tags =
            List.filter (\t -> not <| member small_recent_tags t) tags
    in
        if List.length recent_tags < the_magic_number then
            List.append recent_tags dedup_tags
        else if List.length dedup_tags < the_magic_number then
            List.append small_recent_tags dedup_tags
        else
            List.append small_recent_tags tags


{-|
        situation/
We have new data - pings or categories containing tag data.

        problem/
Tags are just strings so we need to ensure that they are not duplicated.

People want to keep their tags according to what they have entered:
    * If they enter "Dancing" they expect to see "Dancing" and not
      "dancing" or "dAncing"
However, they also:
    * Expect "dancing", "dAncing", and "Dancing" to be the same tag
      and not three different tags

        way/
The mental model the user will carry seem likely to be that the tags
that are held by the categories are the 'master' tags. Hence we will use
these as canonical.
-}
get_canonical_tag : Model -> Tag -> Tag
get_canonical_tag model tag =
    List.foldr
        (\cat accum ->
            List.foldr
                (\cat_tag acc ->
                    if strEq cat_tag tag then
                        cat_tag
                    else
                        acc
                )
                accum
                cat.tags
        )
        tag
        model.cats


{-|
        outcome/
Convert a tag into so that is can hold editing data
-}
editingTagFrom : Model -> Tag -> EditingTag
editingTagFrom model tag =
    let
        cat =
            case getCatForTag model tag of
                Nothing ->
                    ""

                Just c ->
                    c.name
    in
        { orig = tag
        , name = tag
        , cat = cat
        , del = False
        }


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
member : List String -> String -> Bool
member l s =
    List.foldr
        (\str accum ->
            if strEq str s then
                True
            else
                accum
        )
        False
        l


{-|
        outcome/
Do a case insensitive comparison for sorting
-}
case_insensitive_cmp : Tag -> Tag -> Order
case_insensitive_cmp t1 t2 =
    compare (String.toLower t1) (String.toLower t2)


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
        if v == "" || member l v then
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

NB: Both the input lists must be in descending order
-}
sync_merge : (SyncAble a -> SyncAble a -> Order) -> List (SyncAble a) -> List (SyncAble a) -> List (SyncAble a)
sync_merge cmp my svr =
    sync_merge_1 cmp my svr []


sync_merge_1 : (SyncAble a -> SyncAble a -> Order) -> List (SyncAble a) -> List (SyncAble a) -> List (SyncAble a) -> List (SyncAble a)
sync_merge_1 cmp my svr res =
    case ( my, svr ) of
        ( _, [] ) ->
            List.append (List.reverse res) my

        ( [], _ ) ->
            List.append (List.reverse res) svr

        ( h_my :: t_my, h_svr :: t_svr ) ->
            case cmp h_my h_svr of
                GT ->
                    sync_merge_1 cmp t_my svr (h_my :: res)

                LT ->
                    sync_merge_1 cmp my t_svr (h_svr :: res)

                EQ ->
                    case h_my.saved of
                        NewObj ->
                            sync_merge_1 cmp t_my t_svr (h_svr :: res)

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
    ( { consts = consts
      , sch = []
      , cats = []
      , pings = []
      , bricks =
            [ { task = "My most important task for today"
              , tags = [ "tag1", "tag2" ]
              }
            , { task = "My second most important task for today"
              , tags = [ "tag3", "tag4", "justaddtags" ]
              }
            , { task = "A task with no tags is a reminder"
              , tags = []
              }
            , { task = "Sometimes we just need a scratchpad"
              , tags = [ "inbox" ]
              }
            ]
      , get_sch_failed = False
      , get_cats_failed = False
      , get_pings_failed = False
      , after_login = []
      , login_req_in_flight = False
      , last_tick = 0
      , current = Set.empty
      , recent_tags = []
      , all_tags_filter = ""
      , all_tags = []
      , dialog = Nothing
      }
    , Cmd.batch [ Task.perform OnTick Time.now, focusTagInput ]
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
    = SaveNewSch (Result Http.Error Sch)
    | SaveNewCats (Result Http.Error (List Category))
    | SaveNewPings (Result Http.Error (List Ping))
    | AfterLogin (Result Http.Error ())
    | OnTick Time
    | ToggleSelect Int
    | SelectFirstPing
    | AddCurrentTag String
    | RemoveCurrentTag String
    | ServerGotCat String (Result Http.Error ())
    | ServerGotPing Int (Result Http.Error ())
    | HideWindow
    | OnElementFocused (Result Dom.Error ())
    | ShowEditTags
    | EditTagVals EditingTag
    | EditTagName EditingTag String
    | EditTagsDone
    | EditTagsCancel
    | AddBrick Brick
    | ShowEditBricks
    | BricksEdit String
    | EditBricksDone
    | TagInputUpdated String
    | TagInputKeyDown Int
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

        ServerGotCat n r ->
            serverGotCat n r model

        ServerGotPing u r ->
            serverGotPing u r model

        HideWindow ->
            ( model, show "off" )

        OnElementFocused r ->
            onElementFocused r model

        ShowEditTags ->
            editTags model

        EditTagVals etag ->
            editTagVals etag model

        EditTagName etag name ->
            editTagVals { etag | name = name } model

        EditTagsDone ->
            editTagsDone model

        EditTagsCancel ->
            editTagsCancel model

        TagInputUpdated f ->
            tagInputUpdated f model

        TagInputKeyDown key ->
            tagInputKeyDown key model

        AddBrick brick ->
            addBrick brick model

        ShowEditBricks ->
            ( { model | dialog = Just <| EditBricks (bricks2v model) }, Cmd.none )

        BricksEdit v ->
            ( { model | dialog = Just (EditBricks v) }, Cmd.none )

        EditBricksDone ->
            ( { model | dialog = Nothing, bricks = (stringbricks model) }, Cmd.none )

        ShowTODO ->
            ( { model | dialog = Just NotDone }, Cmd.none )

        HideTODO ->
            ( { model | dialog = Nothing }, Cmd.none )


{-|
        understand/
Elm doesn't have a keydown event (hopefully it's coming soon). However
we can create our own event.
-}
onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
    HE.on "keydown" (Decode.map tagger HE.keyCode)


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
        problem/
We want to autofocus the tag entry so that it is easy for the user to
just start typing without using the mouse

        way/
We send a focus request to the id of the input field.

NB: For this to work this id must be used when being displayed in the
view.
-}
focusTagInput : Cmd Msg
focusTagInput =
    Dom.focus tagInputID |> Task.attempt OnElementFocused


tagInputID : String
tagInputID =
    "tag-input"


onElementFocused : Result Dom.Error () -> Model -> ( Model, Cmd Msg )
onElementFocused r model =
    case r of
        Err (Dom.NotFound id) ->
            let
                _ =
                    Debug.log "Unexpected error 823: Unable to focus" id
            in
                ( model, Cmd.none )

        Ok () ->
            ( model, Cmd.none )


{-|
        problem/
We want a useful ping schedule.

        way/
If we have made some ping entries we need the schedule from the first
ping.  Otherwise we get some pings for today. If we already have the
schedule upto a point we request from that point onward.
-}
getSch : Model -> Cmd Msg
getSch model =
    let
        sometime_today_12_hrs_unix =
            (12 * 60 * 60)

        pfrom =
            case List.head (List.reverse model.pings) of
                Just ping ->
                    ping.unix

                Nothing ->
                    tick_to_unix model.last_tick - sometime_today_12_hrs_unix

        from =
            case List.head model.sch of
                Just max_sch ->
                    case List.head (List.reverse model.sch) of
                        Just min_sch ->
                            if max_sch > pfrom && pfrom >= min_sch then
                                max_sch
                            else
                                pfrom

                        Nothing ->
                            pfrom

                Nothing ->
                    pfrom

        u =
            apiURL model.consts "api/sch"

        url =
            u ++ "&from=" ++ Http.encodeUri (toString from)

        req =
            Http.get url decodeSch
    in
        Http.send SaveNewSch req


decodeSch : Decode.Decoder Sch
decodeSch =
    Decode.list Decode.int


{-|
        outcome/
Process the latest ping schedule returned by the server. On error we
mark that an error has taken place so that refreshServerData can try to
reload it again.
Having got the schedule, we check if we need to add any new pings for
the user to enter.
NB: We can optionally dump the schedule for debugging
-}
saveNewSch : Result Http.Error Sch -> Model -> ( Model, Cmd Msg )
saveNewSch r model =
    case r of
        Ok sch ->
            let
                m =
                    { model | sch = sch }

                _ =
                    if gDumpUnix then
                        Debug.log "Schedule"
                            (List.map (\unx -> cal unx ++ " " ++ hhmm unx) sch)
                    else
                        []
            in
                ( addLatestPings m, Cmd.none )

        Err err ->
            onHttpError err { model | get_sch_failed = True } (getSch model)


gDumpUnix : Bool
gDumpUnix =
    False


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
            , sort_order = s_o
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
Process the latest category data returned by the server (merge
categories and clean tags). On error we mark that an error has taken
place so that refreshServerData can try to reload it again.
-}
saveNewCats : Result Http.Error (List Category) -> Model -> ( Model, Cmd Msg )
saveNewCats r model =
    case r of
        Ok cats ->
            let
                u =
                    sync_merge_cats_1 model.cats cats

                m =
                    cleanModelTags { model | cats = u }
            in
                ( m, Cmd.none )

        Err err ->
            onHttpError err { model | get_cats_failed = True } (getCats model.consts)


{-|
        outcome/
So we sort the lists by name (sync_merge needs them in descending
order), merge them together, then re-sort them by the sort order.
-}
sync_merge_cats_1 : List Category -> List Category -> List Category
sync_merge_cats_1 mycats svrcats =
    let
        s_mycats =
            List.reverse <| List.sortBy .name mycats

        s_svrcats =
            List.reverse <| List.sortBy .name svrcats
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
Process the latest ping data returned by the server (merge pings and
clean tags). On error we mark that an error has taken place so that
refreshServerData can try to reload it again.

If we don't have a schedule yet, we are now in a position to make that
call (fill in the schedule between the ping and now) so we launch that
call if needed.
-}
saveNewPings : Result Http.Error (List Ping) -> Model -> ( Model, Cmd Msg )
saveNewPings r model =
    case r of
        Ok pings ->
            let
                u =
                    sync_merge_pings_1 model.pings pings

                m =
                    cleanModelTags { model | pings = u }

                cmd =
                    if List.isEmpty model.sch then
                        getSch m
                    else
                        Cmd.none
            in
                ( m, cmd )

        Err err ->
            onHttpError err { model | get_pings_failed = True } (getPings model.consts)


{-|
        outcome/
So we merge the server and our lists together.
-}
sync_merge_pings_1 : List Ping -> List Ping -> List Ping
sync_merge_pings_1 mypings svrpings =
    sync_merge (\p1 p2 -> compare p1.unix p2.unix) mypings svrpings



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


httpStatusForbidden : Int
httpStatusForbidden =
    403


sameTags : List Tag -> List Tag -> Bool
sameTags l1 l2 =
    case ( l1, l2 ) of
        ( [], [] ) ->
            True

        ( _, [] ) ->
            False

        ( [], _ ) ->
            False

        ( h1 :: t1, h2 :: t2 ) ->
            if h1 == h2 then
                sameTags t1 t2
            else
                False


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
We add past pings to the list. Because the user is NOT expected to use
his memory but only respond with what's being done "right now", if we
were strict we should add only an immediately past ping. But we are more
flexible and will add older pings which will allow the user to enter
things like "sleep" and things they remember for the past couple of
days until their last entry.

        steps/
1. If there are no user pings, get the last ping that is older than the
current time.
2. Otherwise get the list of scheduled pings older than the current time
and younger than the last entered ping.
-}
addLatestPings : Model -> Model
addLatestPings model =
    if List.isEmpty model.pings then
        add_one_ping_1 model
    else
        merge_sch_and_pings_1 model


add_one_ping_1 : Model -> Model
add_one_ping_1 model =
    let
        valid_pings =
            List.filter (\unx -> unix_to_tick unx <= model.last_tick) model.sch
    in
        case List.head valid_pings of
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


merge_1 : Sch -> List Ping -> List Ping
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
Send any updates we have or, if nothing to send, get the latest updates
from the server.
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
                NewObj ->
                    False

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
            [ getSch model, getCats consts, getPings consts ]

        get_on_failure =
            List.map (\( _, cmd ) -> cmd) <|
                List.filter (\( failed, _ ) -> failed)
                    [ ( model.get_sch_failed, getSch model )
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
        _ =
            if gDumpUnix then
                Debug.log "Toggling" (cal unx ++ " " ++ hhmm unx)
            else
                ""

        current =
            if Set.member unx model.current then
                Set.remove unx model.current
            else
                Set.insert unx model.current
    in
        ( loadRecentTags { model | current = current }, Cmd.none )


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


addCurrentTag : String -> Model -> ( Model, Cmd Msg )
addCurrentTag tag model =
    ( addTagToCurrent tag model, Cmd.none )


{-|
        outcome/
Add the tag to the current selections.
-}
addTagToCurrent : String -> Model -> Model
addTagToCurrent tag model =
    let
        add_tag_to_curr p =
            if Set.member p.unix model.current then
                add_tag_1 model tag p
            else
                p

        upd =
            List.map add_tag_to_curr model.pings
    in
        cleanModelTags { model | pings = upd }


{-|
        problem/
Adding a tag is reasonably simple except for two cases:
    1. We do not want to add duplicate tags
    2. Certain tags (like energy tags (-e-high) are exclusive - only one
    can be present at any time.

        outcome/
We will first remove any matching tags then add the canonical version of
tag,  re-sorting the list and marking the ping as updated.
-}
add_tag_1 : Model -> Tag -> Ping -> Ping
add_tag_1 model tag ping =
    let
        matching_tag t =
            if member energyTags tag then
                member energyTags t
            else
                strEq tag t

        not_matching_tag t =
            not (matching_tag t)

        filtered =
            List.filter not_matching_tag ping.tags

        tags =
            get_canonical_tag model tag :: filtered
    in
        { ping | tags = tags, saved = InMemory }


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
Convert the tags to an editing structure and launch the editing tag
dialog window.
-}
editTags : Model -> ( Model, Cmd Msg )
editTags model =
    let
        edit_tags =
            List.map (editingTagFrom model) model.all_tags
    in
        ( { model | dialog = Just (EditTags edit_tags) }, Cmd.none )


{-|
        outcome/
Update the given tag with the new values
-}
editTagVals : EditingTag -> Model -> ( Model, Cmd Msg )
editTagVals etag model =
    case model.dialog of
        Just (EditTags edit_tags) ->
            ( edit_tag_vals_1 etag edit_tags model, Cmd.none )

        _ ->
            ( Debug.log "Unexpected Error: 743" model, Cmd.none )


edit_tag_vals_1 : EditingTag -> List EditingTag -> Model -> Model
edit_tag_vals_1 etag etags model =
    let
        edit_tags =
            List.map
                (\et ->
                    if et.orig == etag.orig then
                        etag
                    else
                        et
                )
                etags
    in
        { model | dialog = Just (EditTags edit_tags) }


{-|
        outcome/
Update all edited tags and categories
-}
editTagsDone : Model -> ( Model, Cmd Msg )
editTagsDone model =
    case model.dialog of
        Just (EditTags edit_tags) ->
            edit_tags_done_1 edit_tags model

        _ ->
            ( Debug.log "Unexpected Error: 743" model, Cmd.none )


edit_tags_done_1 : List EditingTag -> Model -> ( Model, Cmd Msg )
edit_tags_done_1 edit_tags model =
    let
        m =
            model
                |> update_cats_1 edit_tags
                |> update_pings_1 edit_tags

        all_tags =
            all_view_tags m

        u =
            loadRecentTags { m | all_tags = all_tags }
    in
        ( { u | dialog = Nothing }, focusTagInput )


{-|
        outcome/
Empty the existing category tags and repopulate them with all the edited
tags.
-}
update_cats_1 : List EditingTag -> Model -> Model
update_cats_1 edit_tags model =
    let
        cats =
            List.map update_cat model.cats

        update_cat cat =
            let
                tags =
                    get_tags_for cat
            in
                if sameTags cat.tags tags then
                    cat
                else
                    { cat | tags = tags, saved = InMemory }

        get_tags_for cat =
            List.foldr
                (\etag accum ->
                    if strEq etag.cat cat.name then
                        etag.name :: accum
                    else
                        accum
                )
                []
                edit_tags

        m =
            { model | cats = cats }

        clean_cats =
            List.map (cleanTags m) m.cats
    in
        { m | cats = clean_cats }


{-|
        outcome/
Rename any edited tags in the pings
-}
update_pings_1 : List EditingTag -> Model -> Model
update_pings_1 edit_tags model =
    let
        is_name_changed etag =
            etag.orig /= etag.name

        et =
            List.filter is_name_changed edit_tags

        updated_pings =
            List.map update_ping model.pings

        update_ping p =
            let
                tags =
                    List.map rename_tag p.tags
            in
                if sameTags p.tags tags then
                    p
                else
                    cleanTags model { p | tags = tags, saved = InMemory }

        rename_tag tag =
            List.foldr
                (\etag accum ->
                    if strEq etag.orig tag then
                        etag.name
                    else
                        accum
                )
                tag
                et
    in
        { model | pings = updated_pings }


{-|
        outcome/
Mark any updated categories and pings.
-}
mark_updated_entries_1 : Model -> Model -> Model
mark_updated_entries_1 model m =
    let
        is_modified_cat cat =
            case getCat model cat.name of
                Nothing ->
                    False

                Just c ->
                    sameTags c.tags cat.tags

        is_modified_ping ping =
            case getPing model ping.unix of
                Nothing ->
                    False

                Just p ->
                    sameTags p.tags ping.tags

        updated_cats =
            List.map
                (\cat ->
                    if is_modified_cat cat then
                        { cat | saved = InMemory }
                    else
                        cat
                )
                m.cats

        updated_pings =
            List.map
                (\ping ->
                    if is_modified_ping ping then
                        { ping | saved = InMemory }
                    else
                        ping
                )
                m.pings
    in
        { m | cats = updated_cats, pings = updated_pings }


{-|
        outcome/
Discard any editing changes and remove the dialog
-}
editTagsCancel : Model -> ( Model, Cmd Msg )
editTagsCancel model =
    ( { model | dialog = Nothing }, focusTagInput )


{-|
        outcome/
Update the "all tags" list to match the new filter
-}
tagInputUpdated : String -> Model -> ( Model, Cmd Msg )
tagInputUpdated f model =
    if Set.size model.current == 0 then
        selectFirstPing model
    else
        ( { model | all_tags_filter = f }, Cmd.none )


{-|
        outcome/
Handle "enter", "escape", and "tab" keypress in the tag input field
-}
tagInputKeyDown : Int -> Model -> ( Model, Cmd Msg )
tagInputKeyDown key model =
    let
        ( enter, esc, tab ) =
            ( 13, 27, 9 )
    in
        if key == enter then
            add_input_tag_1 model
        else if key == esc then
            clear_input_tag_1 model
        else if key == tab then
            match_existing_tag_1 model
        else
            ( model, Cmd.none )


add_input_tag_1 : Model -> ( Model, Cmd Msg )
add_input_tag_1 model =
    let
        m =
            { model | all_tags_filter = "" }

        add_current_tag =
            addTagToCurrent model.all_tags_filter m
    in
        ( add_current_tag |> loadRecentTags, Cmd.none )


clear_input_tag_1 : Model -> ( Model, Cmd Msg )
clear_input_tag_1 model =
    ( { model | all_tags_filter = "" }, Cmd.none )


{-|
        outcome/
We find the first match and populate the input with that value. Because
tab causes us to spring out of the field we will re-bring focus back.
-}
match_existing_tag_1 : Model -> ( Model, Cmd Msg )
match_existing_tag_1 model =
    let
        get_first_match =
            { model | all_tags_filter = first_match }

        first_match =
            Maybe.withDefault model.all_tags_filter <|
                List.head (filtered_tags model)
    in
        ( get_first_match, focusTagInput )


addBrick : Brick -> Model -> ( Model, Cmd Msg )
addBrick brick model =
    let
        m =
            List.foldr (\t accum -> addTagToCurrent t accum) model brick.tags
    in
        if Set.size model.current == 0 then
            selectFirstPing model
        else
            ( loadRecentTags m, Cmd.none )


bricks2v : Model -> String
bricks2v model =
    String.join "\n\n" (List.map brickstring model.bricks)


brickstring : Brick -> String
brickstring brick =
    String.join " " <|
        brick.task
            :: (List.map (\t -> "#" ++ t) brick.tags)


stringbricks : Model -> List Brick
stringbricks model =
    case model.dialog of
        Just (EditBricks v) ->
            stringbricks_1 v

        _ ->
            Debug.log "Unexpected error 3434" model.bricks


stringbricks_1 : String -> List Brick
stringbricks_1 v =
    let
        lines =
            String.split "\n\n" v

        bricks =
            List.foldr
                (\l accum ->
                    case brick l of
                        Nothing ->
                            accum

                        Just b ->
                            b :: accum
                )
                []
                lines

        brick line =
            let
                tsk =
                    task line

                l2brick =
                    { task = tsk
                    , tags = tags line
                    }
            in
                if String.isEmpty tsk then
                    Nothing
                else
                    Just l2brick

        task line =
            let
                s =
                    String.split "#" line
            in
                case List.head s of
                    Nothing ->
                        ""

                    Just tsk ->
                        String.trim tsk

        tags line =
            let
                rx =
                    Regex.regex "#\\w+"
            in
                Regex.find Regex.All rx line
                    |> List.map .match
                    |> List.map (String.dropLeft 1)
    in
        bricks



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

        outcome/
./ping-mockup.png
-}


type alias ViewParams =
    { scrollbar_offset : Int
    , window :
        { width : Int
        , height : Int
        }
    , friction_bar :
        { icon : String
        , height : Int
        }
    , top_bar :
        { height : Int
        , margin : Int
        , edge_border : String
        , edge_box_shadow : String
        , title_font_size : Int
        , title_font_opacity : String
        , title_font_smoothing_p : String
        , title_font_smoothing_v : String
        }
    , settings_btn :
        { icon : String
        , top : Int
        , left : Int
        , width : Int
        , height : Int
        , opacity : String
        }
    , close_btn :
        { icon : String
        , top : Int
        , left : Int
        , width : Int
        , height : Int
        }
    , done_btn :
        { right : Int
        }
    , ping_list :
        { top : Int
        , right : Int
        , width : Int
        , height : Int
        , edge_border : String
        , edge_box_shadow : String
        , font_size : Int
        }
    , ping_entry :
        { selected_background : String
        , selected_shadow : String
        , background : String
        , height : Int
        , when :
            { width : Int
            , lpad : Int
            }
        , tags :
            { width : String
            , padding : Int
            }
        }
    , edit_area :
        { top : Int
        , left : Int
        , width : Int
        , height : Int
        , color : String
        }
    , category_panel :
        { top : Int
        , width : Int
        , height : Int
        , head_height : Int
        , padding : Int
        , icon_sz : Int
        , head_font_sz : Int
        , body_font_sz : Int
        , body_box : String
        , body_colors : List String
        , header_colors : List String
        , sel_colors : List String
        }
    , category_tag :
        { width : Int
        , padding_top : Int
        , div_padding : Int
        , unsel_padding : Int
        , sel_padding : Int
        , sel_border : String
        , sel_border_radius : Int
        }
    , energy_level :
        { header_color : String
        , body_color : String
        , icon_sel_color : String
        , icon_sz : Int
        , icon_div_padding : Int
        , icon_unsel_padding : String
        , icon_sel_padding : String
        , icon_sel_border : String
        , icon_sel_border_radius : Int
        }
    , tag_selection :
        { top : Int
        , left : Int
        , width : Int
        , height : Int
        , color : String
        , edit :
            { width : Int
            , height : Int
            , icon : String
            }
        , inp :
            { width : Int
            , font_size : Int
            , top : Int
            , left : Int
            , border : String
            , border_radius : Int
            , padding : String
            , outline : String
            }
        , tags :
            { top : Int
            , left : Int
            , width : Int
            , height : Int
            , font_size : Int
            , color : String
            , container_padding : String
            , tag_padding : String
            , sel_border : String
            , sel_border_radius : Int
            , sel_background : String
            }
        }
    , brick_list :
        { top : Int
        , left : Int
        , width : Int
        , height : Int
        , color : String
        , box_shadow : String
        , head_padding : Int
        , head_height : Int
        , head_bg_color : String
        , head_color : String
        , head_font_sz : Int
        , body_padding : Int
        , list_item_left_pad : Int
        , body_font_sz : Int
        , body_line_padding : Int
        , body_line_radius : Int
        }
    , eat_pies :
        { top : Int
        , left : Int
        , width : Int
        , height : Int
        , chart_height : Int
        , legend_top : Int
        , legend_right : Int
        , legend_font_sz : Int
        }
    , bottom_bar :
        { height : Int
        , edge_border : String
        , help_text :
            { margin_right : Int
            , margin_top : Int
            , font_size : Int
            , opacity : String
            }
        }
    , dialog :
        { transition : String
        , not_done :
            { width : Int
            , height : Int
            }
        , edit_tags :
            { width : Int
            , height : Int
            , color : String
            , box_shadow : String
            , title_height : Int
            , title_bgcolor : String
            , title_color : String
            , title_font_sz : Int
            , title_text_shadow : String
            , footer_bgcolor : String
            , footer_height : Int
            , footer_bottom : Int
            , ok_btn_margin_left : Int
            , cancel_btn_margin_left : Int
            , cancel_btn_padding : String
            , tag_card :
                { width : Int
                , box_shadow : String
                , border_radius : Int
                , padding : String
                , uncat_color : String
                , title_height : Int
                , title_font_sz : Int
                , title_border : String
                , title_outline : String
                , chk_font_sz : Int
                , chk_padding : Int
                }
            }
        , edit_bricks :
            { width : Int
            , height : Int
            }
        }
    }


p : ViewParams
p =
    let
        margin_container_left =
            8

        margin_container_right =
            10

        top_bar_icon_sz =
            21

        scrollbar_offset =
            16

        window =
            { width = 800
            , height = 500
            }

        friction_bar =
            { icon = "ping-friction-bar.png"
            , height = 64
            }

        top_bar =
            { height = 64
            , margin = 8
            , edge_border = "1px solid rgb(152, 152, 152)"
            , edge_box_shadow = "rgb(200,200,200) 0px 1px 0px"
            , title_font_size = 14
            , title_font_opacity = "0.8"
            , title_font_smoothing_p = "-webkit-font-smoothing"
            , title_font_smoothing_v = "subpixel-antialiased"
            }

        settings_btn =
            { icon = "ping-settings.png"
            , top = 8
            , left = margin_container_left + top_bar_icon_sz + 8
            , width = top_bar_icon_sz
            , height = top_bar_icon_sz
            , opacity = "0.8"
            }

        close_btn =
            { icon = "ping-close-btn.png"
            , top = 8
            , left = margin_container_left
            , width = top_bar_icon_sz
            , height = top_bar_icon_sz
            }

        done_btn =
            { right = margin_container_right
            }

        ping_list =
            { top = top_bar.height + 1
            , right = 0
            , height = window.height - top_bar.height - bottom_bar.height - 2
            , width = 200
            , edge_border = "1px solid #989898"
            , edge_box_shadow = "#c8c8c8 -1px 1px 0px"
            , font_size = 14
            }

        ping_entry =
            { selected_background = "#277ec1"
            , selected_shadow = "inset 0px -1px 1px #fffdfd"
            , background = "linear-gradient(#d0d0d0, #d9d9d9, #e9e9e9)"
            , height = 48
            , when =
                { width = 64
                , lpad = 8
                }
            , tags =
                { width = "100%"
                , padding = 8
                }
            }

        edit_area =
            { top = ping_list.top + 1
            , left = 0
            , width = window.width - ping_list.width - 2
            , height = ping_list.height - 2
            , color = "white"
            }

        category_panel =
            { top = 0
            , width = edit_area.width // 5
            , height = round (toFloat edit_area.height / golden_ratio)
            , head_height = 80
            , padding = 4
            , icon_sz = 32
            , head_font_sz = 14
            , body_font_sz = 12
            , body_box = "#bbbbbb 0px 0px 1px inset"
            , body_colors =
                [ "linear-gradient(#f4fbe9,#e9f8d3)"
                , "linear-gradient(#f8f8f8, #e1e7ea)"
                , "linear-gradient(#e9fbfb,#d3f8f8)"
                , "linear-gradient(#fff7e5, #fff0cc)"
                ]
            , header_colors =
                [ "linear-gradient(170deg, #84c91d, #85c720, #6ea718)"
                , "linear-gradient(170deg, #d75b65, #f14050, #c52f3c)"
                , "linear-gradient(170deg,#28d7d7, #33dbdc, #21b2b3)"
                , "linear-gradient(170deg,#fca336,#ffb60a,#fb8f0a)"
                ]
            , sel_colors =
                [ "#679a18"
                , "#a72833"
                , "#1e9c9c"
                , "#c78d02"
                ]
            }

        category_tag =
            { width = category_panel.width - 32
            , padding_top = 20
            , div_padding = 11
            , unsel_padding = 5
            , sel_padding =
                4 - 1
                -- subtract 1px for border
            , sel_border = "1px ridge #bbbbbb"
            , sel_border_radius = 4
            }

        energy_level =
            { header_color = "linear-gradient(#02ff9d, #00d985)"
            , body_color = "linear-gradient(#f8f8f8, #e1e7ea)"
            , icon_sel_color = "#d0d0d0"
            , icon_sz = 32
            , icon_div_padding = 11
            , icon_unsel_padding = "4px 6px"
            , icon_sel_padding =
                "3px 5px"
                -- subtract 1px for border
            , icon_sel_border = "1px ridge #929292"
            , icon_sel_border_radius = 2
            }

        tag_selection_height =
            32

        tag_selection_width =
            edit_area.width + 2

        tag_selection_tags_left =
            162

        tag_selection =
            { top = category_panel.top + category_panel.height
            , left = 0
            , width = tag_selection_width
            , height = tag_selection_height
            , color = "linear-gradient(170deg, #606060, #808080, #404040)"
            , edit =
                { width = 40
                , height = tag_selection_height
                , icon = "ping-tags-edit.png"
                }
            , inp =
                { width = 96
                , font_size = 12
                , top = 6
                , left = 42
                , border = "0"
                , border_radius = 8
                , padding = "2px 8px"
                , outline = "none"
                }
            , tags =
                { top = 0
                , left = tag_selection_tags_left
                , width = tag_selection_width - tag_selection_tags_left
                , height = tag_selection_height
                , font_size = 12
                , color = "white"
                , container_padding = "0 4px"
                , tag_padding = "1px 4px 2px 4px"
                , sel_border = "1px ridge #929292"
                , sel_border_radius = 4
                , sel_background = "#333333"
                }
            }

        tag_selection_bottom =
            tag_selection.top + tag_selection.height

        brick_list =
            { top = tag_selection_bottom
            , left = 0
            , width = edit_area.width // 2
            , height = edit_area.height - tag_selection_bottom + 2
            , color = "linear-gradient(#dcdcdc, #b7b7b7)"
            , box_shadow = "1px -1px 1px #444444"
            , head_padding = 4
            , head_height = 16
            , head_font_sz = 12
            , head_bg_color = "black"
            , head_color = "#cccccc"
            , body_padding = 8
            , list_item_left_pad = 20
            , body_font_sz = 12
            , body_line_padding = 4
            , body_line_radius = 2
            }

        eat_pies =
            { top = tag_selection_bottom
            , left = brick_list.width
            , width = edit_area.width - brick_list.width
            , height = edit_area.height - tag_selection_bottom
            , chart_height = 78
            , legend_top = 10
            , legend_right = 0
            , legend_font_sz = 12
            }

        bottom_bar =
            { height = 24
            , edge_border = "1px solid #f0f0f0"
            , help_text = bottom_bar_help_text
            }

        bottom_bar_help_text =
            { margin_right = margin_container_right
            , margin_top = 2
            , font_size = 12
            , opacity = "0.8"
            }

        dialog =
            { transition = "top 350ms ease-in-out"
            , not_done =
                { width = 256
                , height = 96
                }
            , edit_tags =
                { width = window.width - 128
                , height = window.height - 128
                , color = "linear-gradient(to bottom right,#313131,black)"
                , box_shadow = "#909090 -1px -1px 0px 0px"
                , title_height = 48
                , title_bgcolor = "#607D8B"
                , title_color = "#e2e2e2"
                , title_font_sz = 24
                , title_text_shadow = "1px 1px 1px #4c4c4c"
                , footer_bgcolor = "#2a363c"
                , footer_height = 40
                , footer_bottom = 0
                , ok_btn_margin_left = 200
                , cancel_btn_margin_left = 100
                , cancel_btn_padding = "0 8px"
                , tag_card =
                    { width = window.width // 5
                    , box_shadow = "1px 1px black"
                    , border_radius = 8
                    , padding = "4px 4px 8px 4px"
                    , uncat_color = "linear-gradient(170deg, #606060, #626262, #404040)"
                    , title_height = 24
                    , title_font_sz = 12
                    , title_border = "0"
                    , title_outline = "none"
                    , chk_font_sz = 12
                    , chk_padding = 4
                    }
                }
            , edit_bricks =
                { width = 256
                , height = 256
                }
            }
    in
        { scrollbar_offset = scrollbar_offset
        , window = window
        , friction_bar = friction_bar
        , top_bar = top_bar
        , settings_btn = settings_btn
        , close_btn = close_btn
        , done_btn = done_btn
        , ping_list = ping_list
        , ping_entry = ping_entry
        , edit_area = edit_area
        , category_panel = category_panel
        , category_tag = category_tag
        , energy_level = energy_level
        , tag_selection = tag_selection
        , brick_list = brick_list
        , eat_pies = eat_pies
        , bottom_bar = bottom_bar
        , dialog = dialog
        }


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ friction_bar_1
        , top_panel_1
        , settings_btn_1
        , close_btn_1
        , ping_list_1 model
        , edit_area_1 model
        , bottom_bar
        , dialog_box model
        ]


{-|
        outcome/
Show the friction bar on the top which provides a visual cue and allows
us to drag the window
-}
friction_bar_1 : Html.Html Msg
friction_bar_1 =
    let
        style =
            HA.style
                [ ( "background-image", bg p.friction_bar.icon )
                , ( "background-repeat", "no-repeat" )
                , ( "background-attachment", "fixed" )
                , ( "background-position", "center -3px" )
                , ( "width", "100%" )
                , ( "height", px p.friction_bar.height )
                ]
    in
        Html.div [ style, HA.class "tt-dragable" ] []


{-|
        outcome/
The top panel contains the title and action buttons as well as the
"settings" and "close" buttons.
-}
top_panel_1 : Html.Html Msg
top_panel_1 =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", "0" )
                , ( "width", "100%" )
                , ( "height", px p.top_bar.height )
                , ( "border-bottom", p.top_bar.edge_border )
                , ( "box-shadow", p.top_bar.edge_box_shadow )
                ]
    in
        Html.div [ style ] [ subtitle_1, done_btn_1 ]


{-|
        outcome/
Should remind the user that they need to enter what is happening
    *at that very moment*
and not what they were doing a few seconds back or what they are
planning to do immediately after.
-}
subtitle_1 : Html.Html Msg
subtitle_1 =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "bottom", px p.top_bar.margin )
                , ( "width", "100%" )
                , ( "text-align", "center" )
                , ( "font-size", px p.top_bar.title_font_size )
                , ( "font-weight", "bold" )
                , ( "opacity", p.top_bar.title_font_opacity )
                , ( p.top_bar.title_font_smoothing_p, p.top_bar.title_font_smoothing_v )
                ]
    in
        Html.div [ style ]
            [ Html.text "Tag! At This Instant..." ]


{-|
        outcome/
The "done" button is placed at a nice location so the user can "click,
click, click" the tags and then flow to the done button and close the
window.
-}
done_btn_1 : Html.Html Msg
done_btn_1 =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "right", px p.done_btn.right )
                , ( "bottom", px p.top_bar.margin )
                ]
    in
        Html.div [ style, class "button", HE.onClick HideWindow ]
            [ Html.text "Done" ]


{-|
        outcome/
Bring up the settings window
-}
settings_btn_1 : Html.Html Msg
settings_btn_1 =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px p.settings_btn.top )
                , ( "left", px p.settings_btn.left )
                , ( "width", px p.settings_btn.width )
                , ( "height", px p.settings_btn.height )
                , ( "background-image", bg p.settings_btn.icon )
                , ( "background-repeat", "no-repeat" )
                , ( "background-position", "center" )
                , ( "opacity", p.settings_btn.opacity )
                , ( "cursor", "pointer" )
                ]
    in
        Html.div [ style, HE.onClick ShowTODO ] []


{-|
        outcome/
"Close" the window (actually just hides it because we synchronize with
the server in the background and bring up the window again when needed).
-}
close_btn_1 : Html.Html Msg
close_btn_1 =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px p.close_btn.top )
                , ( "left", px p.close_btn.left )
                , ( "width", px p.close_btn.width )
                , ( "height", px p.close_btn.height )
                , ( "background-image", bg p.close_btn.icon )
                , ( "background-repeat", "no-repeat" )
                , ( "background-position", "center" )
                , ( "cursor", "pointer" )
                ]
    in
        Html.div [ style, HE.onClick HideWindow ] []


{-|
        outcome/
Show the list of pings, allowing the user to select and enter the latest
one (and some older ones).
-}
ping_list_1 : Model -> Html.Html Msg
ping_list_1 model =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px p.ping_list.top )
                , ( "right", px p.ping_list.right )
                , ( "height", px p.ping_list.height )
                , ( "width", px p.ping_list.width )
                , ( "border-left", p.ping_list.edge_border )
                , ( "box-shadow", p.ping_list.edge_box_shadow )
                , ( "overflow", "scroll" )
                , ( "font-size", px p.ping_list.font_size )
                , ( "cursor", "pointer" )
                ]

        pings =
            List.take 50 model.pings
    in
        Html.div [ style ]
            [ Html.table
                [ HA.attribute "cellspacing" "0"
                , HA.attribute "cellpadding" "0"
                ]
                (List.map (show_ping_1 model) pings)
            ]


{-|
        outcome/
Show a ping row (colored differently based on selected or not)
-}
show_ping_1 : Model -> Ping -> Html.Html Msg
show_ping_1 model ping =
    let
        style_v =
            if Set.member ping.unix model.current then
                [ ( "background", p.ping_entry.selected_background )
                , ( "box-shadow", p.ping_entry.selected_shadow )
                ]
            else
                [ ( "background", p.ping_entry.background ) ]

        style =
            HA.style <|
                List.append style_v
                    [ ( "height", px p.ping_entry.height ) ]
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
                [ ( "width", px p.ping_entry.when.width )
                , ( "padding-left", px p.ping_entry.when.lpad )
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
                [ ( "width", p.ping_entry.tags.width )
                , ( "padding", px p.ping_entry.tags.padding )
                ]
    in
        Html.td [ style ] [ Html.text tags ]


{-|
        outcome/
The edit area contains the category panels, the tag selection bar, the
'Top Things to do' and the 'Top things worked on today chart'
-}
edit_area_1 : Model -> Html.Html Msg
edit_area_1 model =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px p.edit_area.top )
                , ( "left", px p.edit_area.left )
                , ( "width", px p.edit_area.width )
                , ( "height", px p.edit_area.height )
                , ( "background", p.edit_area.color )
                ]
    in
        Html.div [ style ]
            [ category_panels_1 model
            , tag_selection_bar_1 model
            , brick_list_1 model
            , eat_pies_1 model
            ]


{-|
        outcome/
We show the four categories with all their tags and the "energy-level"
catgory.
-}
category_panels_1 : Model -> Html.Html Msg
category_panels_1 model =
    let
        ndx =
            List.range 0 (List.length model.cats - 1)
    in
        Html.div []
            (energy_level_1 model :: List.map2 (category_panel_1 model) model.cats ndx)


{-|
        understand/
A person's energy level probably has a significant effect on their
productivity. Capturing this allows us to do all sorts of interesting
analysis - what were you doing the day before? what tasks give you the
most boost? etc etc

        outcome/
Show energy levels:
    * High Energy: -e-high
    * Medium Energy: -e-med
    * Low Energy: -e-low

We also count "sleeping" as part of your energy level (as you can't
really evaluate your energy while sleeping)
    * Sleeping: -sleep
-}
energy_level_1 : Model -> Html.Html Msg
energy_level_1 model =
    let
        left =
            p.category_panel.width * 4

        width =
            p.edit_area.width - left

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px p.category_panel.top )
                , ( "left", px left )
                , ( "width", px width )
                , ( "height", px p.category_panel.height )
                ]

        color =
            p.energy_level.header_color

        icon =
            "ping-cat-energy.png"
    in
        Html.div [ style ]
            [ category_head_1 model color icon "Energy Level"
            , energy_level_tags_1 model
            ]


eSleep : String
eSleep =
    "-sleep"


eHigh : String
eHigh =
    "-e-high"


eMed : String
eMed =
    "-e-med"


eLow : String
eLow =
    "-e-low"


energyTags : List String
energyTags =
    [ eSleep, eHigh, eMed, eLow ]


energy_level_tags_1 : Model -> Html.Html Msg
energy_level_tags_1 model =
    let
        height =
            p.category_panel.height - p.category_panel.head_height

        style =
            HA.style
                [ ( "width", "100%" )
                , ( "height", px height )
                , ( "background", p.energy_level.body_color )
                , ( "text-align", "center" )
                , ( "box-shadow", p.category_panel.body_box )
                ]
    in
        Html.div [ style ]
            [ energy_icon_1 model eSleep "ping-energy-sleep.png"
            , energy_icon_1 model eHigh "ping-energy-high.png"
            , energy_icon_1 model eMed "ping-energy-med.png"
            , energy_icon_1 model eLow "ping-energy-low.png"
            ]


{-|
        outcome/
An energy level tag is shown as little icons that the user can click.
Depending on what is selected they will show a different 'selection
status'.
-}
energy_icon_1 : Model -> String -> String -> Html.Html Msg
energy_icon_1 model tag icon =
    let
        style =
            HA.style
                [ ( "display", "block" )
                , ( "margin", "0 auto" )
                , ( "padding-top", px p.energy_level.icon_div_padding )
                , ( "width", px p.energy_level.icon_sz )
                ]

        icon_style =
            [ ( "display", "inline-block" )
            , ( "padding", p.energy_level.icon_unsel_padding )
            , ( "cursor", "pointer" )
            ]

        all_sel_style =
            List.append icon_style
                [ ( "background", p.energy_level.icon_sel_color )
                , ( "border", p.energy_level.icon_sel_border )
                , ( "border-radius", px p.energy_level.icon_sel_border_radius )
                , ( "padding", p.energy_level.icon_sel_padding )
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
                    ( HA.style icon_style, AddCurrentTag tag )

                NoSelection ->
                    ( HA.style icon_style, SelectFirstPing )
    in
        Html.div [ style ]
            [ Html.img [ btn_style, HA.src icon, HE.onClick msg ] [] ]


{-|
        outcome/
The 'head' of a category card
-}
category_head_1 : Model -> String -> String -> String -> Html.Html Msg
category_head_1 model color icon txt =
    let
        iconstyle =
            HA.style
                [ ( "width", px p.category_panel.icon_sz )
                , ( "height", px p.category_panel.icon_sz )
                , ( "display", "inline-block" )
                , ( "padding-top", px p.category_panel.padding )
                ]

        titlestyle =
            HA.style
                [ ( "font-size", px p.category_panel.head_font_sz )
                , ( "padding-top", px p.category_panel.padding )
                ]

        style =
            HA.style
                [ ( "width", "100%" )
                , ( "height", px p.category_panel.head_height )
                , ( "text-align", "center" )
                , ( "background", color )
                ]
    in
        Html.div [ style ]
            [ Html.img [ iconstyle, HA.src icon ] []
            , Html.div [ titlestyle ] [ Html.text txt ]
            ]


category_panel_1 : Model -> Category -> Int -> Html.Html Msg
category_panel_1 model cat ndx =
    let
        left =
            p.category_panel.width * ndx

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px p.category_panel.top )
                , ( "left", px left )
                , ( "width", px p.category_panel.width )
                , ( "height", px p.category_panel.height )
                ]

        color_ndx =
            ndx % (List.length p.category_panel.header_colors)

        head_color =
            Maybe.withDefault "white" <|
                List.head <|
                    List.drop color_ndx p.category_panel.header_colors

        body_color =
            Maybe.withDefault "white" <|
                List.head <|
                    List.drop color_ndx p.category_panel.body_colors

        sel_color =
            Maybe.withDefault "black" <|
                List.head <|
                    List.drop color_ndx p.category_panel.sel_colors

        icon =
            imgURL model.consts cat.icon
    in
        Html.div [ style ]
            [ category_head_1 model head_color icon cat.name
            , category_tags_1 model body_color sel_color cat.tags
            ]


category_tags_1 : Model -> String -> String -> List Tag -> Html.Html Msg
category_tags_1 model bg_color sel_tag_color tags =
    let
        height =
            p.category_panel.height - p.category_panel.head_height

        style =
            HA.style
                [ ( "width", "100%" )
                , ( "height", px height )
                , ( "background", bg_color )
                , ( "font-size", px p.category_panel.body_font_sz )
                , ( "text-align", "center" )
                , ( "overflow", "scroll" )
                , ( "box-shadow", p.category_panel.body_box )
                ]

        sel_tags =
            make_selectable_list model tags
    in
        Html.div [ style ]
            (List.map (category_tag_1 model sel_tag_color) sel_tags)


category_tag_1 : Model -> String -> String -> Html.Html Msg
category_tag_1 model sel_tag_color tag =
    let
        style =
            HA.style
                [ ( "display", "block" )
                , ( "margin", "0 auto" )
                , ( "padding-top", px p.category_tag.div_padding )
                , ( "width", px p.category_tag.width )
                ]

        icon_style =
            [ ( "display", "inline-block" )
            , ( "cursor", "pointer" )
            , ( "padding", px p.category_tag.unsel_padding )
            ]

        all_sel_style =
            List.append icon_style
                [ ( "background", sel_tag_color )
                , ( "border", p.category_tag.sel_border )
                , ( "border-radius", px p.category_tag.sel_border_radius )
                , ( "padding", px p.category_tag.sel_padding )
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
                    ( HA.style icon_style, AddCurrentTag tag )

                NoSelection ->
                    ( HA.style icon_style, SelectFirstPing )
    in
        Html.div [ style ]
            [ Html.span [ btn_style, HE.onClick msg ] [ Html.text tag ] ]


{-|
        outcome/
The tag selection bar consists of an edit button, an input area and a
list of tags.
-}
tag_selection_bar_1 : Model -> Html.Html Msg
tag_selection_bar_1 model =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px p.tag_selection.top )
                , ( "left", px p.tag_selection.left )
                , ( "width", px p.tag_selection.width )
                , ( "height", px p.tag_selection.height )
                , ( "background", p.tag_selection.color )
                , ( "overflow", "hidden" )
                ]
    in
        Html.div [ style ]
            [ edit_button_1
            , tag_input_1 model
            , tags_1 model
            ]


edit_button_1 : Html.Html Msg
edit_button_1 =
    let
        style =
            HA.style
                [ ( "display", "inline-block" )
                , ( "width", px p.tag_selection.edit.width )
                , ( "height", px p.tag_selection.edit.height )
                , ( "background-image", bg p.tag_selection.edit.icon )
                , ( "background-repeat", "no-repeat" )
                , ( "background-position", "center" )
                , ( "cursor", "pointer" )
                ]
    in
        Html.div [ style, class "edit-btn", HE.onClick ShowEditTags ] []


tag_input_1 : Model -> Html.Html Msg
tag_input_1 model =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "width", px p.tag_selection.inp.width )
                , ( "font-size", px p.tag_selection.inp.font_size )
                , ( "top", px p.tag_selection.inp.top )
                , ( "left", px p.tag_selection.inp.left )
                , ( "border", p.tag_selection.inp.border )
                , ( "border-radius", px p.tag_selection.inp.border_radius )
                , ( "padding", p.tag_selection.inp.padding )
                , ( "outline", p.tag_selection.inp.outline )
                ]
    in
        Html.input
            [ style
            , HA.id tagInputID
            , HA.value model.all_tags_filter
            , HE.onInput TagInputUpdated
            , onKeyDown TagInputKeyDown
            ]
            []


{-|
        understand/
It is ridiculously hard to create a nice horizontal list of scrollable
items. Fortunately we can use a combination of the new flexbox model and
table display to achieve the effect.

        outcome/
We filter the user search against all tags and show it in a nice
scrollable list. We make the height slightly bigger to hide the
scrollbar.
-}
tags_1 : Model -> Html.Html Msg
tags_1 model =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px p.tag_selection.tags.top )
                , ( "left", px p.tag_selection.tags.left )
                , ( "width", px p.tag_selection.tags.width )
                , ( "height", px (p.tag_selection.tags.height + p.scrollbar_offset) )
                , ( "display", "flex" )
                , ( "flex-wrap", "nowrap" )
                , ( "overflow-x", "scroll" )
                , ( "overflow-y", "hidden" )
                , ( "font-size", px p.tag_selection.tags.font_size )
                ]

        tags =
            filtered_tags model |> make_selectable_list model
    in
        Html.div [ style ] (List.map (show_tag_1 model) tags)


show_tag_1 : Model -> Tag -> Html.Html Msg
show_tag_1 model tag =
    let
        style =
            HA.style
                [ ( "height", px p.tag_selection.tags.height )
                , ( "line-height", px p.tag_selection.tags.height )
                , ( "padding", p.tag_selection.tags.container_padding )
                , ( "color", p.tag_selection.tags.color )
                , ( "display", "inline-table" )
                , ( "cursor", "pointer" )
                ]

        tag_style =
            [ ( "padding", p.tag_selection.tags.tag_padding ) ]

        all_sel_style =
            List.append tag_style
                [ ( "border", p.tag_selection.tags.sel_border )
                , ( "border-radius", px p.tag_selection.tags.sel_border_radius )
                , ( "background", p.tag_selection.tags.sel_background )
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
                    ( HA.style tag_style, AddCurrentTag tag )

                NoSelection ->
                    ( HA.style tag_style, SelectFirstPing )
    in
        Html.span [ style, HE.onClick msg ]
            [ Html.span [ btn_style ] [ Html.text tag ] ]


{-|
        outcome/
Show the list of bricks to be done for today.
-}
brick_list_1 : Model -> Html.Html Msg
brick_list_1 model =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px p.brick_list.top )
                , ( "left", px p.brick_list.left )
                , ( "width", px p.brick_list.width )
                , ( "height", px p.brick_list.height )
                , ( "box-shadow", p.brick_list.box_shadow )
                ]

        hdr_style =
            HA.style
                [ ( "padding", px p.brick_list.head_padding )
                , ( "height", px p.brick_list.head_height )
                , ( "font-size", px p.brick_list.head_font_sz )
                , ( "background", p.brick_list.head_bg_color )
                , ( "color", p.brick_list.head_color )
                , ( "cursor", "pointer" )
                ]
    in
        Html.div [ style ]
            [ Html.div [ hdr_style, HE.onClick ShowEditBricks ] [ Html.text "To Do" ]
            , brick_list_items_1 model
            ]


brick_list_items_1 : Model -> Html.Html Msg
brick_list_items_1 model =
    let
        height =
            p.brick_list.height
                - p.brick_list.head_height
                - (2 * p.brick_list.head_padding)
                - (2 * p.brick_list.body_padding)

        style =
            HA.style
                [ ( "padding", px p.brick_list.body_padding )
                , ( "padding-left", px p.brick_list.list_item_left_pad )
                , ( "height", px height )
                , ( "overflow", "scroll" )
                , ( "font-size", px p.brick_list.body_font_sz )
                , ( "background", p.brick_list.color )
                ]
    in
        Html.ol [ style ] (List.map brick_1 model.bricks)


brick_1 : Brick -> Html.Html Msg
brick_1 brick =
    let
        style =
            HA.style
                [ ( "padding", px p.brick_list.body_line_padding )
                , ( "cursor", "pointer" )
                , ( "border-radius", px p.brick_list.body_line_radius )
                ]
    in
        Html.li [ style, class "brick-item", HE.onClick (AddBrick brick) ]
            [ Html.text (brickstring brick) ]


{-|
        problem/
Because the ping window is coming up and giving us a chance to be more
aware of what we are doing at the moment, we should provide some more
ways to provide context and help us perhaps decide if there is something
else we should be doing now.

        way/
Give a picture of what we were doing over the last twelve hours. To do
this we will opt for roughly picking the last 16 pings (16 * 45mins = 12 hrs)
and filtering by tags that have happened more than 20% of the time.
That's 3 pings (20% = 2.4 hrs = 3.2 pings ~ 3 pings).

To be outcome-driven we will show the projects we are working on only.
So what we do is filter for project tags, count the number of tags > 3
and subtract from 16 (to classify as 'other').

NB: There are two assumptions we should be clear about here:
    1. Important tags are classified under "Projects"
    2. There is at most one project tag per ping
TODO: Due to these assumptions we need more clarity/discussion on the pie
-}
eat_pies_1 : Model -> Html.Html Msg
eat_pies_1 model =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px p.eat_pies.top )
                , ( "left", px p.eat_pies.left )
                , ( "width", px p.eat_pies.width )
                , ( "height", px p.eat_pies.height )
                , ( "background", "white" )
                , ( "overflow", "scroll" )
                ]
    in
        Html.div [ style ] [ project_pie_1 model ]


project_pie_1 : Model -> Html.Html Msg
project_pie_1 model =
    let
        pings =
            List.take 16 model.pings

        cat_project =
            List.head <|
                List.filter (\c -> strEq c.name "Projects") model.cats

        project_tags =
            case cat_project of
                Nothing ->
                    []

                Just c ->
                    c.tags

        tags =
            List.map .tags pings
                |> List.concat
                |> List.filter (member project_tags)

        tag_counts =
            List.foldr
                (\t accum ->
                    Dict.update t
                        (\v ->
                            case v of
                                Nothing ->
                                    Just 1.0

                                Just n ->
                                    Just (n + 1.0)
                        )
                        accum
                )
                Dict.empty
                tags

        ( pie_data, total ) =
            Dict.foldl
                (\k v ( accum, tot ) -> ( ( v, k ) :: accum, tot + v ))
                ( [], 0 )
                tag_counts

        total_counts =
            if total < 16 then
                ( 16 - total, "-" ) :: pie_data
            else
                pie_data

        chart_style =
            [ ( "height", px p.eat_pies.chart_height ) ]

        legend_style =
            [ ( "position", "absolute" )
            , ( "top", px p.eat_pies.legend_top )
            , ( "right", px p.eat_pies.legend_right )
            , ( "font-size", px p.eat_pies.legend_font_sz )
            ]
    in
        Chart.pie pie_data
            |> Chart.updateStyles "chart" chart_style
            |> Chart.updateStyles "legend" legend_style
            |> Chart.toHtml


{-|
        outcome/
Show the bottom "status-like" bar with help/support link
-}
bottom_bar : Html.Html Msg
bottom_bar =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "bottom", "0" )
                , ( "width", "100%" )
                , ( "height", px p.bottom_bar.height )
                , ( "border-top", p.bottom_bar.edge_border )
                ]
    in
        Html.div [ style ] [ help_txt_1 ]


help_txt_1 : Html.Html Msg
help_txt_1 =
    let
        style =
            HA.style
                [ ( "float", "right" )
                , ( "margin-right", px p.bottom_bar.help_text.margin_right )
                , ( "margin-top", px p.bottom_bar.help_text.margin_top )
                , ( "font-size", px p.bottom_bar.help_text.font_size )
                , ( "opacity", p.bottom_bar.help_text.opacity )
                ]
    in
        Html.a [ style, HA.href "#", HE.onClick ShowTODO ] [ Html.text "(?) Help/Support" ]


{-|
        understand/
When a user is focused on a sub-task (editing tags, categories,
configuration) we bring up an overlay dialog so that they can focus on
that activity. The visual overlay also helps keep them oriented where
they are in the visual flow.

        outcome/
We have an overlay and various dialogs constructed and kept
"out-of-sight" below the visible window. When showing dialogs we
reposition them which allows them to use a smooth CSS transition and
glide in with a nice animation.
-}
dialog_box : Model -> Html.Html Msg
dialog_box model =
    let
        overlay_top_pos =
            0

        out_of_sight =
            1000

        top_pos height =
            (p.window.height - height) // 2
    in
        case model.dialog of
            Nothing ->
                Html.div []
                    [ overlay_1 out_of_sight
                    , not_done_dialog_1 out_of_sight
                    , edit_tags_dialog_1 model [] out_of_sight
                    , edit_bricks_dialog_1 model "" out_of_sight
                    ]

            Just NotDone ->
                Html.div []
                    [ overlay_1 overlay_top_pos
                    , not_done_dialog_1 (top_pos p.dialog.not_done.height)
                    , edit_tags_dialog_1 model [] out_of_sight
                    , edit_bricks_dialog_1 model "" out_of_sight
                    ]

            Just (EditTags edit_tags_data) ->
                Html.div []
                    [ overlay_1 overlay_top_pos
                    , not_done_dialog_1 out_of_sight
                    , edit_tags_dialog_1
                        model
                        edit_tags_data
                        (top_pos p.dialog.edit_tags.height)
                    , edit_bricks_dialog_1 model "" out_of_sight
                    ]

            Just (EditBricks v) ->
                Html.div []
                    [ overlay_1 overlay_top_pos
                    , not_done_dialog_1 out_of_sight
                    , edit_tags_dialog_1 model [] out_of_sight
                    , edit_bricks_dialog_1 model v (top_pos p.dialog.edit_bricks.height)
                    ]


overlay_1 : Int -> Html.Html Msg
overlay_1 top_pos =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "left", "0" )
                , ( "top", px top_pos )
                , ( "width", px p.window.width )
                , ( "height", px p.window.height )
                , ( "background", "black" )
                , ( "opacity", "0.7" )
                ]
    in
        Html.div [ style ] []


{-|
        outcome/
Show a "to be done" message. Should not be required when application is
completed.
-}
not_done_dialog_1 : Int -> Html.Html Msg
not_done_dialog_1 top_pos =
    let
        not_done_left_pos =
            (p.window.width - p.dialog.not_done.width) // 2

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px top_pos )
                , ( "left", px not_done_left_pos )
                , ( "width", px p.dialog.not_done.width )
                , ( "height", px p.dialog.not_done.height )
                , ( "background-color", "white" )
                , ( "border-radius", "8px" )
                , ( "border", "1px solid black" )
                , ( "transition", p.dialog.transition )
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


{-|
        outcome/
Show a window that allows users to edit tags. This contains all the tags
matching the user filter with their categories and options for renaming
and deleting.
-}
edit_tags_dialog_1 : Model -> List EditingTag -> Int -> Html.Html Msg
edit_tags_dialog_1 model edit_tags_data top_pos =
    let
        edit_tags_left_pos =
            (p.window.width - p.dialog.edit_tags.width) // 2

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px top_pos )
                , ( "left", px edit_tags_left_pos )
                , ( "width", px p.dialog.edit_tags.width )
                , ( "height", px p.dialog.edit_tags.height )
                , ( "background", p.dialog.edit_tags.color )
                , ( "box-shadow", p.dialog.edit_tags.box_shadow )
                , ( "transition", p.dialog.transition )
                ]

        tags =
            filtered_edit_tags model edit_tags_data
    in
        Html.div [ style ]
            [ edit_tags_dialog_title_1
            , edit_tags_dialog_tags_1 model tags
            , edit_tags_dialog_footer_1
            ]


edit_tags_dialog_title_1 : Html.Html Msg
edit_tags_dialog_title_1 =
    let
        edit_tags =
            p.dialog.edit_tags

        style =
            HA.style
                [ ( "width", px edit_tags.width )
                , ( "height", px edit_tags.title_height )
                , ( "line-height", px edit_tags.title_height )
                , ( "background", edit_tags.title_bgcolor )
                , ( "color", edit_tags.title_color )
                , ( "font-size", px edit_tags.title_font_sz )
                , ( "text-shadow", edit_tags.title_text_shadow )
                , ( "text-align", "center" )
                ]
    in
        Html.div [ style ]
            [ Html.text "Edit Tags" ]


edit_tags_dialog_tags_1 : Model -> List EditingTag -> Html.Html Msg
edit_tags_dialog_tags_1 model tags =
    let
        top =
            p.dialog.edit_tags.title_height

        width =
            p.dialog.edit_tags.width

        footer_height =
            p.dialog.edit_tags.footer_height

        height =
            p.dialog.edit_tags.height - top - footer_height

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px top )
                , ( "width", px width )
                , ( "height", px height )
                , ( "overflow", "scroll" )
                ]
    in
        Html.div [ style ]
            (List.map (edit_tags_dialog_tag_1 model) tags)


edit_tags_dialog_tag_1 : Model -> EditingTag -> Html.Html Msg
edit_tags_dialog_tag_1 model tag =
    let
        width =
            p.dialog.edit_tags.tag_card.width

        padding =
            width // 5

        tag_card =
            p.dialog.edit_tags.tag_card

        style =
            HA.style
                [ ( "display", "inline-block" )
                , ( "margin", px padding )
                , ( "width", px width )
                ]

        inp_style =
            HA.style
                [ ( "display", "block" )
                , ( "width", px width )
                , ( "height", px tag_card.title_height )
                , ( "text-align", "center" )
                , ( "border", tag_card.title_border )
                , ( "outline", tag_card.title_outline )
                , ( "border-top-left-radius", px tag_card.border_radius )
                , ( "border-top-right-radius", px tag_card.border_radius )
                , ( "font-weight", "bold" )
                ]

        cat_names =
            "" :: List.map .name model.cats

        colors =
            tag_card.uncat_color :: p.category_panel.header_colors

        color_names =
            List.map2 (\name color -> ( name, color )) cat_names colors

        ( _, color ) =
            Maybe.withDefault ( tag.cat, "red" ) <|
                List.head <|
                    List.filter (\( n, c ) -> strEq n tag.cat) color_names

        body_style =
            HA.style
                [ ( "background", color )
                , ( "border-bottom-left-radius", px tag_card.border_radius )
                , ( "border-bottom-right-radius", px tag_card.border_radius )
                , ( "padding", tag_card.padding )
                ]

        chk_style =
            [ ( "display", "block" )
            , ( "font-size", px tag_card.chk_font_sz )
            , ( "padding", px tag_card.chk_padding )
            ]

        chk_sel_style =
            List.append chk_style
                [ ( "font-weight", "bold" )
                , ( "font-style", "italic" )
                ]

        chk_txt_style =
            HA.style
                [ ( "padding", px tag_card.chk_padding )
                , ( "cursor", "pointer" )
                ]

        chks =
            List.map chkFrom cat_names

        chk_default_name n =
            if String.isEmpty n then
                "(not categorized)"
            else
                n

        chkFrom n =
            if strEq n tag.cat then
                Html.div [ HA.style chk_sel_style ]
                    [ Html.input [ HA.type_ "radio", HA.checked True ] []
                    , Html.span [ chk_txt_style ] [ Html.text (chk_default_name n) ]
                    ]
            else
                let
                    msg =
                        EditTagVals { tag | cat = n }
                in
                    Html.div [ HA.style chk_style ]
                        [ Html.input
                            [ HA.type_ "radio"
                            , HE.onClick msg
                            ]
                            []
                        , Html.span
                            [ chk_txt_style
                            , HE.onClick msg
                            ]
                            [ Html.text (chk_default_name n) ]
                        ]
    in
        Html.div [ style ]
            [ Html.input
                [ inp_style
                , HA.value tag.name
                , HE.onInput (EditTagName tag)
                ]
                []
            , Html.div [ body_style ] chks
            ]


edit_tags_dialog_footer_1 : Html.Html Msg
edit_tags_dialog_footer_1 =
    let
        edit_tags =
            p.dialog.edit_tags

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "bottom", px edit_tags.footer_bottom )
                , ( "width", px edit_tags.width )
                , ( "height", px edit_tags.footer_height )
                , ( "background", edit_tags.footer_bgcolor )
                , ( "display", "flex" )
                , ( "align-items", "center" )
                ]

        ok_btn =
            Html.div
                [ HA.style
                    [ ( "margin-left"
                      , px
                            edit_tags.ok_btn_margin_left
                      )
                    ]
                , class "button"
                , HE.onClick EditTagsDone
                ]
                [ Html.text "Update Tags" ]

        cancel_btn =
            Html.div
                [ HA.style
                    [ ( "margin-left"
                      , px
                            edit_tags.cancel_btn_margin_left
                      )
                    , ( "padding", edit_tags.cancel_btn_padding )
                    ]
                , class "button"
                , HE.onClick EditTagsCancel
                ]
                [ Html.text "Discard Changes" ]
    in
        Html.div [ style ] [ ok_btn, cancel_btn ]


edit_bricks_dialog_1 : Model -> String -> Int -> Html.Html Msg
edit_bricks_dialog_1 model v top_pos =
    let
        left_pos =
            (p.window.width - p.dialog.edit_bricks.width) // 2

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px top_pos )
                , ( "left", px left_pos )
                , ( "width", px p.dialog.edit_bricks.width )
                , ( "height", px p.dialog.edit_bricks.height )
                , ( "background", p.dialog.edit_tags.color )
                , ( "padding", "24px" )
                , ( "transition", p.dialog.transition )
                ]

        ta_style =
            HA.style
                [ ( "width", px (p.dialog.edit_bricks.width - 16) )
                , ( "height", px (p.dialog.edit_bricks.height - 16 - 48) )
                , ( "padding", "8px" )
                ]

        ok_btn =
            Html.div [ class "button", HE.onClick EditBricksDone ] [ Html.text "Ok" ]
    in
        Html.div [ style ]
            [ Html.textarea [ ta_style, HA.value v, HE.onInput BricksEdit ] []
            , ok_btn
            ]



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

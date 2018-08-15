port module Ping exposing (main)

import Html
import Html.Attributes as HA
import Html.Events as HE
import Date
import Time
import Dom
import Task
import Dom.Scroll as Scroll
import Json.Decode as Decode
import Chart
import Dict
import Set


{-|
        understand/
Main entry point of our program. Note that here we only declare our
framework entry functions and do nothing else
-}
main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


{-|
        understand/
The main model of TEA - The Elm Architecture. Everything happening in
our system (except for CSS animations) can be derived from this model so
it will have a hodge-podge of everything we need to keep track of.
-}
type alias Model =
    { now : Unix
    , pingset : List DailyPings
    , bricks : List Brick
    , cats : List Category
    , tags : List Tag
    , top_tags : List Tag
    , current : List Unix
    , shift_on : Bool
    , shift_start : Maybe Unix
    , ctrl_on : Bool
    , input_tags : String
    , input_tags_direct_entry : String
    , ac_ndx : Int
    , input_tag_cat : String
    , dialog : Maybe Dialog
    }


{-|
        understand/
A gathering of pings that fall on the same day
-}
type alias DailyPings =
    { unix : Unix
    , pings : List Ping
    }


{-|
        understand/
A 'Ping' represents a sampling of what the user was doing at a given
unix time (at a given sampling 'gap' in seconds and a comment/context)
-}
type alias Ping =
    { unix : Unix
    , tags : List Tag
    , gap : Int
    , ctx : List String
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
A "tag" represents a category of things being done at a given ping
-}
type alias Tag =
    String


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
    , tags : List Tag
    , top_tags : List Tag
    }


{-|
        understand/
List of dialog boxes to show
-}
type Dialog
    = NotDone
    | Loading
    | EditTags (List EditingTag)
    | EditBricks String
    | CategorizeTag Tag
    | KBShorts


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
    { orig :
        { name : Tag
        , cat : String
        }
    , name : Tag
    , cat : String
    , del : Bool
    }


{-|
        outcome/
Convert a tag into an EditingTag so that is can hold editing data
-}
editingTagFrom : Model -> Tag -> EditingTag
editingTagFrom model tag =
    let
        cat =
            List.foldr
                (\c acc ->
                    if member c.tags tag then
                        c.name
                    else
                        acc
                )
                ""
                model.cats
    in
        { orig =
            { name = tag
            , cat = cat
            }
        , name = tag
        , cat = cat
        , del = False
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


specialTag : Tag -> Bool
specialTag t =
    List.member t energyTags


displayTags : List Tag -> List Tag
displayTags tags =
    List.filter (\t -> not (specialTag t)) tags


{-|
        outcome/
Initialize our model to start up.
-}
init : ( Model, Cmd Msg )
init =
    ( { now = 0
      , pingset = []
      , bricks = []
      , cats = []
      , tags = []
      , top_tags = []
      , current = []
      , shift_on = False
      , shift_start = Nothing
      , ctrl_on = False
      , input_tags = ""
      , input_tags_direct_entry = ""
      , ac_ndx = -1
      , input_tag_cat = ""
      , dialog = Just Loading
      }
    , Cmd.none
    )



{--
        understand/
Messages are how the model gets updated. They are clean and structured
way to centralize all changes. This gives us the following benefits:
1. No changes happen somewhere deep inside a code flow
2. Fewer unexpected side effects happen when we change or remove a
function because coupling should be reduced.
-}


type Msg
    = SetNow Unix
    | SetLatestDailyPings (List DailyPings)
    | SetLatestCats (List Category)
    | SetLatestBricks (List Brick)
    | SetLatestTags (List Tag)
    | SetLatestTopTags (List Tag)
    | SetLatestSelection (List Unix)
    | LoadingDone Bool
    | OnSplKeyActivated String
    | OnSplKeyReleased String
    | OnSplKeysReleased Bool
    | OnPrepareToHide Bool
    | ClickSelect Unix
    | SelectFirstPing
    | AddCurrentTag String
    | RemoveCurrentTag String
    | SetTagCat Tag String
    | HideWindow
    | OnScrollTagsUp (Result Dom.Error (List ()))
    | OnElementFocused (Result Dom.Error ())
    | ShowEditTags
    | EditTagVals EditingTag
    | EditTagName EditingTag String
    | ToggleTagDel EditingTag
    | EditTagsDone
    | EditTagsCancel
    | AddBrick Brick
    | ShowEditBricks
    | BricksEdit String
    | EditBricksDone
    | TagInputUpdated String
    | TagInputKeyDown Int
    | TagCatInputUpdated String
    | TagCatInputKeyDown Tag Int
    | ShowTODO
    | HideTODO
    | RepeatLast Ping


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNow unix ->
            ( { model | now = unix }, Cmd.none )

        SetLatestDailyPings pingset ->
            ( { model | pingset = pingset }, Cmd.none )

        SetLatestCats cats ->
            ( { model | cats = cats }, Cmd.none )

        SetLatestBricks bricks ->
            ( { model | bricks = bricks }, Cmd.none )

        SetLatestTags tags ->
            ( { model | tags = tags }, Cmd.none )

        SetLatestTopTags tags ->
            ( { model | top_tags = tags }, Cmd.none )

        SetLatestSelection sels ->
            ( { model | current = sels }, Cmd.none )

        LoadingDone _ ->
            finishedLoading model

        TagInputUpdated f ->
            tagInputUpdated f model

        TagInputKeyDown key ->
            tagInputKeyDown key model

        TagCatInputUpdated f ->
            tagCatInputUpdated f model

        TagCatInputKeyDown tag key ->
            tagCatInputKeyDown tag key model

        OnSplKeyActivated key ->
            onSplKeyUpDown True key model

        OnSplKeyReleased key ->
            onSplKeyUpDown False key model

        OnSplKeysReleased _ ->
            onSplKeysReleased model

        OnPrepareToHide _ ->
            onPrepareToHide model

        ClickSelect unx ->
            clickSelect unx model

        SelectFirstPing ->
            selectFirstPing model

        AddCurrentTag tag ->
            addUserTag tag model

        RemoveCurrentTag tag ->
            ( model, rmTagsFromSel [ tag ] )

        SetTagCat tag cat ->
            dialogDone { model | input_tag_cat = "" } (tagCatSet tag cat)

        HideWindow ->
            ( model, hideWindowCmds )

        OnScrollTagsUp r ->
            onScrollTagsUp r model

        OnElementFocused r ->
            onElementFocused r model

        ShowEditTags ->
            editTags model

        EditTagVals etag ->
            editTagVals etag model

        EditTagName etag name ->
            editTagVals { etag | name = name } model

        ToggleTagDel etag ->
            editTagVals { etag | del = not etag.del } model

        EditTagsDone ->
            editTagsDone model

        EditTagsCancel ->
            dialogDone model Cmd.none

        AddBrick brick ->
            addBrick brick model

        ShowEditBricks ->
            ( { model | dialog = Just <| EditBricks (bricks2v model) }, Cmd.none )

        BricksEdit v ->
            ( { model | dialog = Just (EditBricks v) }, Cmd.none )

        EditBricksDone ->
            dialogDone model (setBricks (stringbricks model))

        ShowTODO ->
            ( { model | dialog = Just NotDone }, Cmd.none )

        HideTODO ->
            dialogDone model Cmd.none

        RepeatLast ping ->
            repeatLast ping model


finishedLoading : Model -> ( Model, Cmd Msg )
finishedLoading model =
    case model.dialog of
        Just Loading ->
            dialogDone model Cmd.none

        _ ->
            ( model, Cmd.none )


{-|
        outcome/
Update the "all tags" list to match the new filter
-}
tagInputUpdated : String -> Model -> ( Model, Cmd Msg )
tagInputUpdated f model =
    ( { model | input_tags = f, input_tags_direct_entry = f, ac_ndx = -1 }, Cmd.none )


{-|
        outcome/
Handle "enter", "escape", and "tab" keypress in the tag input field
-}
tagInputKeyDown : Int -> Model -> ( Model, Cmd Msg )
tagInputKeyDown key model =
    let
        ( enter, esc, tab ) =
            ( 13, 27, 9 )

        clear_inputs =
            { model | input_tags = "", input_tags_direct_entry = "", ac_ndx = -1 }
    in
        if key == enter then
            addUserTag model.input_tags clear_inputs
        else if key == esc then
            ( clear_inputs, Cmd.none )
        else if key == tab then
            match_existing_tag_1 model
        else
            ( model, Cmd.none )


{-|
        outcome/
Add user tag to current selection in model. If the tag has no category
pop up a window allowing the user to incrementally categorize at this
point
-}
addUserTag : Tag -> Model -> ( Model, Cmd Msg )
addUserTag tag model =
    let
        is_categorized_tag =
            List.foldr
                (\cat accum ->
                    List.foldr
                        (\t acc ->
                            if strEq t tag then
                                True
                            else
                                acc
                        )
                        accum
                        cat.tags
                )
                False
                model.cats
    in
        if strEq "" tag then
            openKBShorts model
        else if is_categorized_tag || specialTag tag then
            ( model, addTagsToSel [ tag ] )
        else
            ( { model | dialog = Just (CategorizeTag tag) }, focusTagCatInput )


openKBShorts : Model -> ( Model, Cmd Msg )
openKBShorts model =
    ( { model | dialog = Just KBShorts }, stopKeyHandling True )


{-|
        outcome/
Update the "category tag" input entry
-}
tagCatInputUpdated : String -> Model -> ( Model, Cmd Msg )
tagCatInputUpdated f model =
    ( { model | input_tag_cat = f }, Cmd.none )


{-|
        outcome/
Handle "enter" and "escape"
-}
tagCatInputKeyDown : Tag -> Int -> Model -> ( Model, Cmd Msg )
tagCatInputKeyDown tag key model =
    let
        ( enter, esc ) =
            ( 13, 27 )
    in
        if key == enter then
            set_tag_from_input_cat_1 tag model
        else if key == esc then
            dialogDone { model | input_tag_cat = "" } Cmd.none
        else
            ( model, Cmd.none )


{-|
        problem/
As the user types we would like to quickly narrow down the matching
categories.

        way/
Return the list of categories that match the start of what the user has
entered.
-}
getMatchingCats : Model -> List String
getMatchingCats model =
    let
        pfx =
            String.toLower <| String.trim model.input_tag_cat

        matches ( n1, n2, _ ) =
            let
                n_1 =
                    String.toLower n1

                n_2 =
                    String.toLower n2
            in
                String.startsWith pfx n_1 || String.startsWith pfx n_2
    in
        if String.isEmpty pfx then
            []
        else
            List.map (\( n, _, _ ) -> n) <| List.filter matches show_cats


{-|
        outcome/
If the user has narrowed the input down to a single matching category we
return that. Othewise nothing
-}
getMatchingCat : Model -> Maybe String
getMatchingCat model =
    let
        cats =
            getMatchingCats model
    in
        case List.head cats of
            Nothing ->
                Nothing

            Just cat ->
                if List.length cats == 1 then
                    Just cat
                else
                    Nothing


set_tag_from_input_cat_1 : Tag -> Model -> ( Model, Cmd Msg )
set_tag_from_input_cat_1 tag model =
    let
        m =
            { model | input_tag_cat = "" }
    in
        case getMatchingCat model of
            Nothing ->
                dialogDone m (addTagsToSel [ tag ])

            Just c ->
                dialogDone m (tagCatSet tag c)


{-|
        outcome/
When the user selects a category for the tag we add it to the current
selection as well as the category, and bring focus back to the input.
-}
tagCatSet : Tag -> String -> Cmd Msg
tagCatSet tag cat =
    Cmd.batch
        [ addTagToCat ( tag, cat )
        , focusTagInput
        , addTagsToSel [ tag ]
        ]


{-|
        outcome/
We rotate among the filtered tags so that the user can just hit enter
and accept them.
-}
match_existing_tag_1 : Model -> ( Model, Cmd Msg )
match_existing_tag_1 model =
    let
        matches =
            filtered_tags model

        ac_ndx =
            if model.shift_on then
                if model.ac_ndx >= 0 then
                    model.ac_ndx - 1
                else
                    List.length matches - 1
            else if List.length matches - 1 > model.ac_ndx then
                model.ac_ndx + 1
            else
                -1

        tag =
            if ac_ndx == -1 then
                model.input_tags_direct_entry
            else
                List.drop ac_ndx matches
                    |> List.head
                    |> Maybe.withDefault model.input_tags_direct_entry
    in
        ( { model | input_tags = tag, ac_ndx = ac_ndx }, focusTagInput )


{-|
        outcome/
Keep track of the user pressing and releasing special keys - shift and
control
-}
onSplKeyUpDown : Bool -> String -> Model -> ( Model, Cmd Msg )
onSplKeyUpDown activated key model =
    if key == "Shift" then
        ( { model | shift_on = activated }, Cmd.none )
    else if key == "Control" || key == "Meta" then
        ( { model | ctrl_on = activated }, Cmd.none )
    else if activated then
        dialogKeysHandler key model
    else
        ( model, Cmd.none )


{-|
            outcome/
Handle special keys for any active dialog
-}
dialogKeysHandler : String -> Model -> ( Model, Cmd Msg )
dialogKeysHandler key model =
    case model.dialog of
        Just (EditTags _) ->
            edittags_keyshandler key model

        Just KBShorts ->
            kbshorts_keyshandler key model

        _ ->
            ( model, Cmd.none )


edittags_keyshandler : String -> Model -> ( Model, Cmd Msg )
edittags_keyshandler key model =
    if key == "Enter" then
        editTagsDone model
    else if key == "Escape" then
        dialogDone model Cmd.none
    else
        ( model, Cmd.none )


{-|
        outcome/
Update all edited tags and categories
-}
editTagsDone : Model -> ( Model, Cmd Msg )
editTagsDone model =
    case model.dialog of
        Just (EditTags edit_tags) ->
            dialogDone model (tagsEdited edit_tags)

        _ ->
            ( model, Cmd.none )


kbshorts_keyshandler : String -> Model -> ( Model, Cmd Msg )
kbshorts_keyshandler key model =
    if key == "Enter" then
        dialogDone model hideWindowCmds
    else if key == "Escape" then
        dialogDone model Cmd.none
    else if key == "r" || key == "R" then
        if List.length (kbshorts_sel_tags model) == 0 then
            case kbshorts_repeat_ping model of
                Nothing ->
                    ( model, Cmd.none )

                Just p ->
                    ( model, addTagsToSel p.tags )
        else
            ( model, Cmd.none )
    else if key == "1" then
        ( model, addTagsToSel [ eHigh ] )
    else if key == "2" then
        ( model, addTagsToSel [ eMed ] )
    else if key == "3" then
        ( model, addTagsToSel [ eLow ] )
    else if key == "4" then
        ( model, addTagsToSel [ eSleep ] )
    else
        ( model, Cmd.none )


kbshorts_sel_tags : Model -> List Tag
kbshorts_sel_tags model =
    List.map (alwaysGetPing model) model.current
        |> List.map .tags
        |> List.concat
        |> Set.fromList
        |> Set.toList


kbshorts_repeat_ping : Model -> Maybe Ping
kbshorts_repeat_ping model =
    let
        from =
            List.minimum model.current
    in
        case from of
            Nothing ->
                Nothing

            Just unx ->
                List.reverse model.pingset
                    |> List.map (\dp -> List.reverse dp.pings)
                    |> List.concat
                    |> List.filter
                        (\p -> p.unix < unx && List.length (displayTags p.tags) > 0)
                    |> List.head


onSplKeysReleased : Model -> ( Model, Cmd Msg )
onSplKeysReleased model =
    ( { model | shift_on = False, ctrl_on = False }, Cmd.none )


{-|
        outcome/
The window is about to be hidden. When it next shows it should look like
it's a newly appearing window. For this, we reset the selections to
null, focus the input box, and scroll everything back to the top.
-}
onPrepareToHide : Model -> ( Model, Cmd Msg )
onPrepareToHide model =
    ( { model | current = [], shift_start = Nothing, dialog = Nothing }
    , Cmd.batch [ scrollListsToTop_1, stopKeyHandling False, focusTagInput ]
    )


{-|
        outcome/
Handle ping selection nicely using the standard paradigm - click picks a
new item, shift-click picks a range, and control-click adds an item.
Because the user has changed the current selection, also update the
recent tags list.

        steps/
If ctrl is pressed add this selection.
If shift is pressed create a new set of selections from the starting
selection to this one. If there is no starting selection, make this one the
start of the selection.
Otherwise make this the selection.
-}
clickSelect : Unix -> Model -> ( Model, Cmd Msg )
clickSelect unx model =
    let
        ( current, m ) =
            if model.shift_on then
                shift_select_1 model unx
            else if model.ctrl_on then
                ctrl_select_1 model unx
            else
                ( [ unx ], { model | shift_start = Just unx } )
    in
        ( m, setSel current )


shift_select_1 : Model -> Unix -> ( List Int, Model )
shift_select_1 model unx =
    let
        shift_start =
            if List.length model.current == 1 then
                List.head model.current
            else
                model.shift_start

        m =
            { model | shift_start = shift_start }

        sel_start =
            case shift_start of
                Nothing ->
                    unx

                Just h ->
                    h

        dir_to_walk =
            if sel_start < unx then
                List.foldl
            else
                List.foldr

        sels =
            dir_to_walk
                (\p acc ->
                    if sel_start < unx then
                        if p.unix <= unx && p.unix >= sel_start then
                            p.unix :: acc
                        else
                            acc
                    else if p.unix <= sel_start && p.unix >= unx then
                        p.unix :: acc
                    else
                        acc
                )
                []
                (xtractPings m)
    in
        ( sels, m )


ctrl_select_1 : Model -> Unix -> ( List Int, Model )
ctrl_select_1 model unx =
    let
        in_sel =
            List.member unx model.current
    in
        if in_sel then
            ( List.filter (\u -> u /= unx) model.current
            , { model | shift_start = Nothing }
            )
        else
            ( unx :: model.current
            , { model | shift_start = Just unx }
            )


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
            List.head <| List.reverse <| xtractPings model
    in
        case first_ping of
            Nothing ->
                ( model, Cmd.none )

            Just p ->
                ( model, setSel [ p.unix ] )


{-|
        outcome/
Convert the tags to an editing structure and launch the editing tag
dialog window.
-}
editTags : Model -> ( Model, Cmd Msg )
editTags model =
    let
        edit_tags =
            List.map (editingTagFrom model) <|
                List.filter (\t -> not (specialTag t)) model.tags
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
Close the dialog, executing any commands, restoring key handling and
refocusing the main input field.
-}
dialogDone : Model -> Cmd Msg -> ( Model, Cmd Msg )
dialogDone model cmd =
    ( { model | dialog = Nothing }
    , Cmd.batch [ cmd, stopKeyHandling False, focusTagInput ]
    )


hideWindowCmds : Cmd Msg
hideWindowCmds =
    Cmd.batch [ show "off", setSel [] ]


addBrick : Brick -> Model -> ( Model, Cmd Msg )
addBrick brick model =
    ( model, addTagsToSel brick.tags )


bricks2v : Model -> String
bricks2v model =
    String.join "\n\n" (List.map .task model.bricks)


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

        brick line =
            let
                tsk =
                    String.trim line
            in
                if String.isEmpty tsk then
                    Nothing
                else
                    Just { task = tsk, tags = [] }
    in
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


{-|
        understand/
In addition to user interaction and HTTP events, we can "subscribe" to
push events - timers, websockets, and javascript communication.
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ latestTime SetNow
        , latestDailyPings SetLatestDailyPings
        , latestCats SetLatestCats
        , latestBricks SetLatestBricks
        , latestTags SetLatestTags
        , latestTopTags SetLatestTopTags
        , latestSel SetLatestSelection
        , loadingDone LoadingDone
        , splKeyActive OnSplKeyActivated
        , splKeyReleased OnSplKeyReleased
        , splKeysReleased OnSplKeysReleased
        , prepareToHide OnPrepareToHide
        ]



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


{-|
        understand/
When showing some dialogs we would like to comprehensively handle all
keys (without default behaviour). In other cases (say where there are
text-boxes or input boxes) we want the default behaviour. For this we
inform javascript to preventDefault or not.
-}
port stopKeyHandling : Bool -> Cmd msg


{-|
        understand/
Javascript has finished the initial loading
-}
port loadingDone : (Bool -> msg) -> Sub msg


{-|
        understand/
We send our updates to javascript and receive updated data.
-}
port latestTime : (Unix -> msg) -> Sub msg


port latestDailyPings : (List DailyPings -> msg) -> Sub msg


port latestCats : (List Category -> msg) -> Sub msg


port latestBricks : (List Brick -> msg) -> Sub msg


port latestTags : (List Tag -> msg) -> Sub msg


port latestTopTags : (List Tag -> msg) -> Sub msg


port latestSel : (List Unix -> msg) -> Sub msg


port setSel : List Unix -> Cmd msg


port addTagsToSel : List Tag -> Cmd msg


port rmTagsFromSel : List Tag -> Cmd msg


port addTagToCat : ( Tag, String ) -> Cmd msg


port tagsEdited : List EditingTag -> Cmd msg


port setBricks : List Brick -> Cmd msg


{-|
        understand/
In order to get the status of special keys - shift, control, alt, meta,
arrowleft... - we use javascript as it has much better support than elm.
-}
port splKeyActive : (String -> msg) -> Sub msg


port splKeyReleased : (String -> msg) -> Sub msg


port splKeysReleased : (Bool -> msg) -> Sub msg


{-|
        understand/
Just before the window is going to be hidden we get a signal so we can
prepare the window for the next display.
-}
port prepareToHide : (Bool -> msg) -> Sub msg


{-|
        understand/
Javascript errors are sent to us so we can show them etc
-}
port errormsg : (String -> msg) -> Sub msg


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
        outcome/
Similart to `focusTagInput` but we want to focus on the Tag Category
field
-}
focusTagCatInput : Cmd Msg
focusTagCatInput =
    Dom.focus tagCatInputID |> Task.attempt OnElementFocused


tagCatInputID : String
tagCatInputID =
    "tag-cat-input"


{-|
        outcome/
Scroll the ping list and all the category tags to the top so the user
can see the most appropriate ones first.

NB: The ping list and category tags must each be given a unique id
(categories from 0 to 3) which are then used here.
-}
scrollListsToTop_1 : Cmd Msg
scrollListsToTop_1 =
    let
        cat_ids =
            List.map catTagsId [ 0, 1, 2, 3 ]

        scroll_cmds =
            List.map Scroll.toTop (pingListID :: cat_ids)
    in
        Task.sequence scroll_cmds |> Task.attempt OnScrollTagsUp


catTagsId : Int -> String
catTagsId num =
    "cat-tags-" ++ toString num


pingListID : String
pingListID =
    "ping-list"


onScrollTagsUp : Result Dom.Error (List ()) -> Model -> ( Model, Cmd Msg )
onScrollTagsUp r model =
    case r of
        Err (Dom.NotFound id) ->
            let
                _ =
                    Debug.log "Unexpected error 478: Unable to scroll" id
            in
                ( model, Cmd.none )

        Ok _ ->
            ( model, Cmd.none )


{-|
        understand/
Elm doesn't have a keydown event (hopefully it's coming soon). However
we can create our own event.
-}
onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
    HE.on "keydown" (Decode.map tagger HE.keyCode)



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
    , repeat_btn :
        { right : Int
        , width : Int
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
        , sleepy_background : String
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
        , loading :
            { icon_width : Int
            , title_font_sz : Int
            , subtitle_font_sz : Int
            , subtitle_font_weight : String
            , subtitle_color : String
            , subtitle_margin : Int
            , subtitle_left : Int
            }
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
                , icon :
                    { width : Int
                    , top : Int
                    , left : Int
                    }
                , del :
                    { width : Int
                    , top : Int
                    , right : Int
                    }
                }
            }
        , edit_bricks :
            { width : Int
            , height : Int
            }
        , cat_curr :
            { top : Int
            , left : Int
            , width : Int
            , height : Int
            , inp_width : Int
            , background : String
            , padding : Int
            , border_radius : Int
            , box_shadow : String
            , title :
                { height : Int }
            , line :
                { height : Int
                , margin_top : Int
                , padding : Int
                , icon_width : Int
                }
            }
        , kb_shorts :
            { width : Int
            , height : Int
            , background : String
            , padding : Int
            , font_size : Int
            , border_radius : Int
            , box_shadow : String
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

        repeat_btn =
            { right = done_btn.right + 86
            , width = 96
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
            , sleepy_background = "linear-gradient(#7b7c9d, #8a8aa8, #9899b3)"
            , height = 48
            , when =
                { width = 32
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
            , loading =
                { icon_width = 41
                , title_font_sz = 64
                , subtitle_font_sz = 12
                , subtitle_font_weight = "200"
                , subtitle_color = "grey"
                , subtitle_margin = 8
                , subtitle_left = 8
                }
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
                    , icon =
                        { width = 32
                        , top = 86
                        , left = 112
                        }
                    , del =
                        { width = 16
                        , top = -20
                        , right = 10
                        }
                    }
                }
            , edit_bricks =
                { width = 256
                , height = 256
                }
            , cat_curr =
                { top = cat_curr_top
                , left = cat_curr_left
                , width = cat_curr_width
                , height = cat_curr_height
                , inp_width = cat_curr_inp_width
                , background = "linear-gradient(white, #cacaca)"
                , padding = 10
                , border_radius = 6
                , box_shadow = "1px 1px 3px 0px black"
                , title = { height = cat_curr_title_height }
                , line =
                    { height = cat_curr_line_height
                    , margin_top = cat_curr_line_margin_top
                    , padding = cat_curr_line_padding
                    , icon_width = 24
                    }
                }
            , kb_shorts =
                { width = 300
                , height = 300
                , background = "linear-gradient(white, #cacaca)"
                , padding = 24
                , font_size = 14
                , border_radius = 4
                , box_shadow = "1px 1px 3px 0px black"
                }
            }

        cat_curr_top =
            (window.height - cat_curr_height) // 2

        cat_curr_left =
            (window.width - cat_curr_width) // 2

        cat_curr_width =
            150

        cat_curr_inp_width =
            128

        cat_curr_line_margin_top =
            8

        cat_curr_line_padding =
            2

        cat_curr_height =
            cat_curr_title_height
                + (cat_curr_line_height
                    + cat_curr_line_margin_top
                    + cat_curr_line_padding
                  )
                * List.length show_cats
                + 8

        cat_curr_title_height =
            48

        cat_curr_line_height =
            32
    in
        { scrollbar_offset = scrollbar_offset
        , window = window
        , friction_bar = friction_bar
        , top_bar = top_bar
        , settings_btn = settings_btn
        , close_btn = close_btn
        , done_btn = done_btn
        , repeat_btn = repeat_btn
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
        , top_panel_1 model
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
top_panel_1 : Model -> Html.Html Msg
top_panel_1 model =
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
        Html.div [ style ] [ subtitle_1, repeat_last_1 model, done_btn_1 ]


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
            [ Html.text "Tag! At This Instant I am..." ]


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
The "repeat" button activates if the current ping is empty allowing it
to copy the previous entry.
-}
repeat_last_1 : Model -> Html.Html Msg
repeat_last_1 model =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "right", px p.repeat_btn.right )
                , ( "width", px p.repeat_btn.width )
                , ( "bottom", px p.top_bar.margin )
                ]

        active p =
            Html.div [ style, class "button", HE.onClick (RepeatLast p) ]
                [ Html.text "Repeat Last" ]

        inactive =
            Html.div [ style, class "inactive-btn" ]
                [ Html.text "Repeat Last" ]
    in
        if List.length (kbshorts_sel_tags model) == 0 then
            case kbshorts_repeat_ping model of
                Nothing ->
                    inactive

                Just p ->
                    active p
        else
            inactive


repeatLast : Ping -> Model -> ( Model, Cmd Msg )
repeatLast ping model =
    ( model, addTagsToSel ping.tags )


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
    in
        Html.div [ style, HA.id pingListID ]
            [ Html.table
                [ HA.attribute "cellspacing" "0"
                , HA.attribute "cellpadding" "0"
                , HA.attribute "width" "100%"
                ]
                (show_dailypings_1 model)
            ]


show_dailypings_1 : Model -> List (Html.Html Msg)
show_dailypings_1 model =
    let
        tr_style =
            HA.style
                [ ( "height", "24px" ) ]

        td_style =
            HA.style
                [ ( "text-align", "center" )
                ]

        style =
            HA.style
                [ ( "background", "#0b81da" )
                , ( "padding", "2px 8px" )
                , ( "border-radius", "4px" )
                , ( "font-size", "12px" )
                , ( "font-style", "italic" )
                , ( "color", "aliceblue" )
                , ( "box-shadow", "1px 1px 1px #909090" )
                ]
    in
        List.foldr
            (\dailypings acc ->
                List.append acc <|
                    Html.tr [ tr_style ]
                        [ Html.td [ HA.colspan 2, td_style ]
                            [ Html.span [ style ]
                                [ Html.text (ddmmyy model.now dailypings.unix) ]
                            ]
                        ]
                        :: (List.map (show_ping_1 model) <| List.reverse dailypings.pings)
            )
            []
            model.pingset


{-|
        outcome/
Show a ping row (colored differently based on selected or not)
-}
show_ping_1 : Model -> Ping -> Html.Html Msg
show_ping_1 model ping =
    let
        style_v =
            if List.member ping.unix model.current then
                [ ( "background", p.ping_entry.selected_background )
                , ( "box-shadow", p.ping_entry.selected_shadow )
                ]
            else if List.member eSleep ping.tags then
                [ ( "background", p.ping_entry.sleepy_background )
                , ( "font-style", "italic" )
                , ( "box-shadow", p.ping_entry.selected_shadow )
                ]
            else
                [ ( "background", p.ping_entry.background ) ]

        style =
            HA.style <|
                List.append style_v
                    [ ( "height", px p.ping_entry.height ) ]
    in
        Html.tr [ style, HE.onClick (ClickSelect ping.unix) ]
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
    tagsRow Html.td ping.tags


tagsRow : (List (Html.Attribute Msg) -> List (Html.Html Msg) -> Html.Html Msg) -> List String -> Html.Html Msg
tagsRow elem tags =
    let
        e_color_map =
            [ ( eSleep, "rgba(85, 86, 143, 0.75)" )
            , ( eHigh, "rgba(0, 191, 118, 0.75)" )
            , ( eMed, "rgba(113, 89, 89, 0.75)" )
            , ( eLow, "rgba(235, 17, 36, 0.52)" )
            ]

        energy_border =
            List.foldr
                (\( e, clr ) acc ->
                    if List.member e tags then
                        "8px solid " ++ clr
                    else
                        acc
                )
                "none"
                e_color_map

        t =
            String.join ", " (displayTags tags)

        style =
            HA.style
                [ ( "padding", px p.ping_entry.tags.padding )
                , ( "border-right", energy_border )
                ]
    in
        elem [ style ] [ Html.text t ]


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
We show the five categories with all their tags.
-}
category_panels_1 : Model -> Html.Html Msg
category_panels_1 model =
    let
        cats =
            get_model_cats_1 model.cats

        ndx =
            List.range 0 (List.length cats - 1)
    in
        Html.div []
            (energy_level_1 model :: List.map2 (category_panel_1 model) cats ndx)


{-|
        outcome/
We display four categories in this view:
    * Activities
    * Projects
    * People
    * Places
Find and return these categories from our list.
-}
show_cats : List ( String, String, String )
show_cats =
    [ ( "Activities", "Activity", "ping-cat-activities.png" )
    , ( "Projects", "Project", "ping-cat-projects.png" )
    , ( "People", "Person", "ping-cat-people.png" )
    , ( "Places", "Place", "ping-cat-places.png" )
    ]


get_model_cats_1 : List Category -> List ( Category, String )
get_model_cats_1 cats =
    List.map
        (\( n, _, i ) ->
            let
                matching =
                    List.filter (\c -> strEq c.name n) cats
            in
                case List.head matching of
                    Nothing ->
                        ( { name = n, tags = [], top_tags = [] }, i )

                    Just c ->
                        ( c, i )
        )
        show_cats


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
            [ category_head_1 color icon "Energy Level"
            , energy_level_tags_1 model
            ]


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
            [ energy_icon_1 model eHigh "ping-energy-high.png"
            , energy_icon_1 model eMed "ping-energy-med.png"
            , energy_icon_1 model eLow "ping-energy-low.png"
            , energy_icon_1 model eSleep "ping-energy-sleep.png"
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
category_head_1 : String -> String -> String -> Html.Html Msg
category_head_1 color icon txt =
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


category_panel_1 : Model -> ( Category, String ) -> Int -> Html.Html Msg
category_panel_1 model ( cat, icon ) ndx =
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
            ndx % List.length p.category_panel.header_colors

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
    in
        Html.div [ style ]
            [ category_head_1 head_color icon cat.name
            , category_tags_1
                model
                body_color
                sel_color
                (selectableTags cat.tags cat.top_tags 4)
                ndx
            ]


category_tags_1 : Model -> String -> String -> List Tag -> Int -> Html.Html Msg
category_tags_1 model bg_color sel_tag_color tags ndx =
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
    in
        Html.div [ style, HA.id (catTagsId ndx) ]
            (List.map (category_tag_1 model sel_tag_color) tags)


{-|
            outcome/
Show each category tag as a click-able button. If the tag is empty show
it as a divider.
-}
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
        if tag == "" then
            Html.div [ style ] [ Html.span [] [ Html.text "*" ] ]
        else
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
            , HA.value model.input_tags
            , HE.onInput TagInputUpdated
            , onKeyDown TagInputKeyDown
            , HA.autofocus True
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

NB: We only show the top 'n' tags because otherwise it gets too slow
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
            List.take 128 <| filtered_tags model

        ndxs =
            List.range 0 (List.length tags)
    in
        Html.div [ style ] (List.map2 (show_tag_1 model) tags ndxs)


show_tag_1 : Model -> Tag -> Int -> Html.Html Msg
show_tag_1 model tag ndx =
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

        divider_style =
            HA.style
                [ ( "padding", p.tag_selection.tags.container_padding )
                , ( "line-height", px p.tag_selection.tags.height )
                , ( "color", p.tag_selection.tags.color )
                , ( "display", "inline-table" )
                , ( "font-size", "1.4em" )
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

        ( btn_press_style, msg ) =
            case selstatus of
                InAll ->
                    ( all_sel_style, RemoveCurrentTag tag )

                InSome ->
                    ( some_sel_style, AddCurrentTag tag )

                InNone ->
                    ( tag_style, AddCurrentTag tag )

                NoSelection ->
                    ( tag_style, SelectFirstPing )

        btn_style =
            if ndx == model.ac_ndx then
                let
                    stl =
                        List.filter (\( s, _ ) -> s /= "border") btn_press_style
                in
                    HA.style (( "border", "1px dashed yellow" ) :: stl)
            else
                HA.style btn_press_style
    in
        if tag == "" then
            Html.div [ divider_style ] [ Html.span [] [ Html.text "|" ] ]
        else
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
            [ Html.div [ hdr_style, HE.onClick ShowEditBricks ] [ Html.text "Most Important Tasks" ]
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
            [ Html.text brick.task ]


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
            List.take 16 <| xtractPings model

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

        full_pie_data =
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
        Chart.pie full_pie_data
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
TODO: Animate the dialog in using something like the following strategy.
Load an overlay and create the dialog box "out-of-sight" below the
visible window. Then hand control back to the browser to render and,
on the next tick, position the dialog correctly allowing it to use a
smooth CSS transition and glide in with a nice animation.
-}
dialog_box : Model -> Html.Html Msg
dialog_box model =
    let
        top_pos height =
            (p.window.height - height) // 2
    in
        case model.dialog of
            Nothing ->
                Html.div [] []

            Just Loading ->
                Html.div [] [ overlay_1, loading_dialog_1 ]

            Just NotDone ->
                Html.div []
                    [ overlay_1
                    , not_done_dialog_1 (top_pos p.dialog.not_done.height)
                    ]

            Just (EditTags edit_tags_data) ->
                Html.div []
                    [ overlay_1
                    , edit_tags_dialog_1
                        model
                        edit_tags_data
                        (top_pos p.dialog.edit_tags.height)
                    ]

            Just (EditBricks v) ->
                Html.div []
                    [ overlay_1
                    , edit_bricks_dialog_1 v (top_pos p.dialog.edit_bricks.height)
                    ]

            Just (CategorizeTag tag) ->
                Html.div [] [ overlay_1, categorize_current_tag_1 tag model ]

            Just KBShorts ->
                Html.div []
                    [ overlay_1
                    , kb_shorts_1 (top_pos p.dialog.kb_shorts.height) model
                    ]


overlay_1 : Html.Html Msg
overlay_1 =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "left", "0" )
                , ( "top", "0" )
                , ( "width", px p.window.width )
                , ( "height", px p.window.height )
                , ( "background", "black" )
                , ( "opacity", "0.7" )
                ]
    in
        Html.div [ style ] []


{-|
        outcome/
Show the 'loading' icon
-}
loading_dialog_1 : Html.Html Msg
loading_dialog_1 =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", "0" )
                , ( "width", px p.window.width )
                , ( "height", px p.window.height )
                , ( "background", "white" )
                , ( "opacity", "0.9" )
                , ( "display", "flex" )
                , ( "align-items", "center" )
                , ( "justify-content", "center" )
                , ( "text-align", "center" )
                ]

        img_style =
            HA.style
                [ ( "width", px p.dialog.loading.icon_width )
                ]

        title_style =
            HA.style
                [ ( "font-size", px p.dialog.loading.title_font_sz )
                , ( "font-family", "'Sacramento', cursive" )
                ]

        subtitle_style =
            HA.style
                [ ( "font-size", px p.dialog.loading.subtitle_font_sz )
                , ( "font-weight", p.dialog.loading.subtitle_font_weight )
                , ( "color", p.dialog.loading.subtitle_color )
                , ( "margin", px p.dialog.loading.subtitle_margin )
                , ( "position", "relative" )
                , ( "left", px p.dialog.loading.subtitle_left )
                ]
    in
        Html.div [ style ]
            [ Html.div []
                [ Html.div [ title_style ] [ Html.text "Tag Time" ]
                , Html.img [ img_style, HA.src "ping-loading.gif" ] []
                , Html.div [ subtitle_style ] [ Html.text "Loading . . ." ]
                ]
            ]


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
TODO: Large set of tags take so long to appear that it hangs. For now we
limit the number of tags to a couple of hundred max.
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
            List.take 512 <|
                filtered_edit_tags model edit_tags_data
    in
        Html.div [ style ]
            [ edit_tags_dialog_title_1
            , edit_tags_dialog_tags_1 tags
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


edit_tags_dialog_tags_1 : List EditingTag -> Html.Html Msg
edit_tags_dialog_tags_1 tags =
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
            (List.map edit_tags_dialog_tag_1 tags)


edit_tags_dialog_tag_1 : EditingTag -> Html.Html Msg
edit_tags_dialog_tag_1 tag =
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

        txt_dec =
            if tag.del then
                "line-through"
            else
                "none"

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
                , ( "text-decoration", txt_dec )
                ]

        all_cats =
            ( "", "(not categorized)", "ping-cat-empty.png" ) :: show_cats

        colors =
            tag_card.uncat_color :: p.category_panel.header_colors

        color_names_icons =
            List.map2 (\( n, _, i ) color -> ( n, color, i )) all_cats colors

        ( _, color, icon ) =
            Maybe.withDefault ( tag.cat, "red", "ping-cat-unk.png" ) <|
                List.head <|
                    List.filter (\( n, _, _ ) -> strEq n tag.cat) color_names_icons

        body_style =
            HA.style
                [ ( "position", "relative" )
                , ( "background", color )
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
            List.map chkFrom all_cats

        icon_style =
            HA.style
                [ ( "position", "absolute" )
                , ( "width", px tag_card.icon.width )
                , ( "top", px tag_card.icon.top )
                , ( "left", px tag_card.icon.left )
                ]

        del_style =
            HA.style
                [ ( "position", "absolute" )
                , ( "width", px tag_card.del.width )
                , ( "top", px tag_card.del.top )
                , ( "right", px tag_card.del.right )
                ]

        cnt =
            Html.img
                [ HA.src "ping-tag-del.png"
                , del_style
                , HE.onClick (ToggleTagDel tag)
                ]
                []
                :: Html.img [ HA.src icon, icon_style ] []
                :: chks

        chkFrom ( n, txt, _ ) =
            if strEq n tag.cat then
                Html.div [ HA.style chk_sel_style ]
                    [ Html.input [ HA.type_ "radio", HA.checked True ] []
                    , Html.span [ chk_txt_style ] [ Html.text txt ]
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
                            [ Html.text txt ]
                        ]
    in
        Html.div [ style ]
            [ Html.input
                [ inp_style
                , HA.value tag.name
                , HE.onInput (EditTagName tag)
                ]
                []
            , Html.div [ body_style ] cnt
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


edit_bricks_dialog_1 : String -> Int -> Html.Html Msg
edit_bricks_dialog_1 v top_pos =
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


categorize_current_tag_1 : Tag -> Model -> Html.Html Msg
categorize_current_tag_1 tag model =
    let
        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px p.dialog.cat_curr.top )
                , ( "left", px p.dialog.cat_curr.left )
                , ( "width", px p.dialog.cat_curr.width )
                , ( "height", px p.dialog.cat_curr.height )
                , ( "background", p.dialog.cat_curr.background )
                , ( "padding", px p.dialog.cat_curr.padding )
                , ( "border-radius", px p.dialog.cat_curr.border_radius )
                , ( "box-shadow", p.dialog.cat_curr.box_shadow )
                ]
    in
        Html.div [ style ]
            [ cat_curr_title_1 tag model
            , cat_curr_cats_1 tag model
            ]


cat_curr_title_1 : Tag -> Model -> Html.Html Msg
cat_curr_title_1 tag model =
    let
        style =
            HA.style
                [ ( "width", px p.dialog.cat_curr.width )
                , ( "height", px p.dialog.cat_curr.title.height )
                , ( "overflow", "hidden" )
                ]

        inp_style =
            HA.style
                [ ( "display", "block" )
                , ( "width", px p.dialog.cat_curr.inp_width )
                , ( "margin", "2px auto" )
                ]
    in
        Html.div [ style ]
            [ Html.div [] [ Html.text <| tag ++ " is a:" ]
            , Html.input
                [ HA.id tagCatInputID
                , HA.value model.input_tag_cat
                , HE.onInput TagCatInputUpdated
                , onKeyDown (TagCatInputKeyDown tag)
                , inp_style
                ]
                []
            ]


cat_curr_cats_1 : Tag -> Model -> Html.Html Msg
cat_curr_cats_1 tag model =
    let
        style =
            HA.style
                [ ( "width", "100%" )
                ]
    in
        Html.div [ style ] (List.map (cat_curr_cat_1 tag model) show_cats)


cat_curr_cat_1 : Tag -> Model -> ( String, String, String ) -> Html.Html Msg
cat_curr_cat_1 tag model ( name, singular, icon ) =
    let
        sel_style =
            HA.style
                [ ( "height", px p.dialog.cat_curr.line.height )
                , ( "line-height", px p.dialog.cat_curr.line.height )
                , ( "margin-top", px p.dialog.cat_curr.line.margin_top )
                , ( "padding", px p.dialog.cat_curr.line.padding )
                , ( "padding", "2px" )
                , ( "cursor", "pointer" )
                , ( "font-weight", "bold" )
                , ( "background", "grey" )
                ]

        unsel_style =
            HA.style
                [ ( "height", px p.dialog.cat_curr.line.height )
                , ( "line-height", px p.dialog.cat_curr.line.height )
                , ( "margin-top", px p.dialog.cat_curr.line.margin_top )
                , ( "padding", px p.dialog.cat_curr.line.padding )
                , ( "cursor", "pointer" )
                ]

        style =
            if List.member name (getMatchingCats model) then
                sel_style
            else
                unsel_style

        icon_style =
            HA.style
                [ ( "width", px p.dialog.cat_curr.line.icon_width )
                , ( "padding-right", "4px" )
                ]
    in
        Html.div
            [ class "tag-cat"
            , style
            , HE.onClick (SetTagCat tag name)
            ]
            [ Html.img [ icon_style, HA.src icon ] []
            , Html.text singular
            ]


kb_shorts_1 : Int -> Model -> Html.Html Msg
kb_shorts_1 top model =
    let
        left_pos =
            (p.window.width - p.dialog.kb_shorts.width) // 2

        style =
            HA.style
                [ ( "position", "absolute" )
                , ( "top", px top )
                , ( "left", px left_pos )
                , ( "width", px p.dialog.kb_shorts.width )
                , ( "height", px p.dialog.kb_shorts.height )
                , ( "background", p.dialog.kb_shorts.background )
                , ( "padding", px p.dialog.kb_shorts.padding )
                , ( "font-size", px p.dialog.kb_shorts.font_size )
                , ( "border-radius", px p.dialog.kb_shorts.border_radius )
                , ( "box-shadow", p.dialog.kb_shorts.box_shadow )
                ]
    in
        Html.div [ style ]
            [ kb_shorts_title_1 model
            , kb_shorts_tags_1 model
            , kb_shorts_energy_1 model
            , kb_shorts_dismissed_1 model
            , kb_shorts_icon_1 model
            ]


kb_shorts_title_1 : Model -> Html.Html Msg
kb_shorts_title_1 model =
    let
        style =
            HA.style
                [ ( "padding-bottom", "16px" ) ]
    in
        Html.div [ style ]
            [ Html.text "Use any of the following keys to modify the current entry" ]


kb_shorts_tags_1 : Model -> Html.Html Msg
kb_shorts_tags_1 model =
    let
        tags =
            kbshorts_sel_tags model

        style =
            HA.style
                [ ( "font-style", "italic" )
                , ( "max-height", "32px" )
                , ( "overflow", "scroll" )
                , ( "text-align", "center" )
                , ( "padding", "8px" )
                , ( "box-shadow", "inset 0 0 1px 1px #9E9E9E" )
                ]
    in
        if List.length tags == 0 then
            kb_shorts_repeat_1 model
        else
            kb_shorts_tagsrow_1 False tags


kb_shorts_repeat_1 : Model -> Html.Html Msg
kb_shorts_repeat_1 model =
    case kbshorts_repeat_ping model of
        Nothing ->
            Html.div [] []

        Just p ->
            let
                style =
                    HA.style [ ( "padding-bottom", px 12 ) ]
            in
                Html.div []
                    [ Html.div [ style ]
                        [ kb_shorts_row_1 "R" "Repeat previous tags" ]
                    , kb_shorts_tagsrow_1 True p.tags
                    ]


kb_shorts_tagsrow_1 : Bool -> List Tag -> Html.Html Msg
kb_shorts_tagsrow_1 copy tags =
    let
        opacity =
            if copy then
                "0.5"
            else
                "1.0"

        style =
            HA.style
                [ ( "background", "linear-gradient(to bottom right, #f6fdfd, #e9fbfb)" )
                , ( "height", px 48 )
                , ( "width", px 256 )
                , ( "overflow", "scroll" )
                , ( "margin", "0 auto" )
                , ( "opacity", opacity )
                , ( "box-shadow", "0 0 1px 0px black" )
                ]
    in
        Html.div [ style ] [ tagsRow Html.div tags ]


kb_shorts_row_1 : String -> String -> Html.Html Msg
kb_shorts_row_1 key msg =
    let
        first_style =
            HA.style
                [ ( "display", "inline-block" )
                , ( "width", px 128 )
                ]

        key_style =
            HA.style
                [ ( "font-weight", "bold" ) ]

        snd_style =
            HA.style
                [ ( "display", "inline-block" ) ]
    in
        Html.div []
            [ Html.div [ first_style ]
                [ Html.span [] [ Html.text "Press " ]
                , Html.span [ key_style ] [ Html.text key ]
                , Html.span [] [ Html.text " : " ]
                ]
            , Html.div [ snd_style ] [ Html.text msg ]
            ]


kb_shorts_energy_1 : Model -> Html.Html Msg
kb_shorts_energy_1 model =
    let
        style =
            HA.style [ ( "padding-top", px 24 ) ]
    in
        Html.div [ style ]
            [ kb_shorts_row_1 "1" "High Energy"
            , kb_shorts_row_1 "2" "Medium Energy"
            , kb_shorts_row_1 "3" "Low Energy"
            , kb_shorts_row_1 "4" "Asleep"
            ]


kb_shorts_dismissed_1 : Model -> Html.Html Msg
kb_shorts_dismissed_1 model =
    let
        style =
            HA.style [ ( "padding-top", px 24 ) ]
    in
        Html.div [ style ]
            [ kb_shorts_row_1 "Esc" "Back"
            , kb_shorts_row_1 "Enter" "Done & Close"
            ]


kb_shorts_icon_1 : Model -> Html.Html Msg
kb_shorts_icon_1 model =
    let
        style =
            HA.style
                [ ( "width", "32px" )
                , ( "position", "absolute" )
                , ( "bottom", "18px" )
                , ( "right", "18px" )
                , ( "opacity", "0.9" )
                ]
    in
        Html.img [ style, HA.src "login-icon-322x256.png" ] []



{--
        understand/
View helper functions
-}


golden_ratio : Float
golden_ratio =
    1.61803399


px : Int -> String
px v =
    toString v ++ "px"


bg : String -> String
bg i =
    "url(" ++ i ++ ")"


pad2 : Int -> String
pad2 v =
    if v < 10 then
        "0" ++ toString v
    else
        toString v


ddmmyy : Unix -> Unix -> String
ddmmyy now unix =
    let
        date =
            unix_to_local unix

        n =
            unix_to_local now

        y =
            unix_to_local (now - 60 * 60 * 24)

        is_today_1 =
            Date.day date
                == Date.day n
                && Date.month date
                == Date.month n
                && Date.year date
                == Date.year n

        is_yesterday_1 =
            Date.day date
                == Date.day y
                && Date.month date
                == Date.month y
                && Date.year date
                == Date.year y
    in
        if is_today_1 then
            "Today"
        else if is_yesterday_1 then
            "Yesterday"
        else
            toString (Date.day date)
                ++ "/"
                ++ toString (Date.month date)
                ++ " "
                ++ toString (Date.year date)


hhmm : Int -> String
hhmm unix =
    let
        date =
            unix_to_local unix
    in
        pad2 (Date.hour date)
            ++ ":"
            ++ pad2 (Date.minute date)


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
            normalize model.input_tags_direct_entry

        matches tag =
            if specialTag tag then
                False
            else if String.isEmpty f then
                True
            else
                String.contains f (normalize tag)

        tags =
            List.filter matches model.tags

        top_tags =
            List.filter matches model.top_tags
    in
        selectableTags tags top_tags 10


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
            normalize model.input_tags_direct_entry

        matches etag =
            if String.isEmpty f then
                True
            else
                String.contains f (normalize etag.orig.name)
    in
        List.filter matches etags



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
        (xtractPings model)


xtractPings : Model -> List Ping
xtractPings model =
    List.foldl
        (\pl acc ->
            List.append acc pl.pings
        )
        []
        model.pingset


alwaysGetPing : Model -> Int -> Ping
alwaysGetPing model unx =
    case getPing model unx of
        Nothing ->
            { unix = unx, tags = [], ctx = [], gap = 0 }

        Just p ->
            p


{-|
        outcome/
Check if the given tag is in the current selection and return the status
-}
tagInSelection : Model -> String -> SelStatus
tagInSelection model tag =
    let
        sel_pings =
            List.map (alwaysGetPing model) model.current

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
We would like to have recently used tags ("top tags") to come up first
followed by an alphabetical list. This makes it easy for the user to
pick from the recent tags but also go ahead and search easily when the
number of tags increases.

        problem/
In cases when the list is small the list looks like this:
    code tagtime act code tagtime
    |--recent--| |--alphabetical--|

This kind of repetition looks strange and wrong.

        way/
When we have a certain number of items in the alphabetical list (to
display) and a certain number of items in the selection list (say 5) -
it no longer looks strange:
    code design tagtime office exercise act code cook design tagtime...
    |---------  recent ---------------| |----------- alphabetical ---

So what we will do is show the top and alphabetical lists as long as
there are enough items and just the alphabetical list if there are not.
We put a blank tag between to display a visual break between top tags
and all tags.
-}
selectableTags : List Tag -> List Tag -> Int -> List Tag
selectableTags tags top_tags num_display =
    if List.length tags >= num_display && List.length top_tags >= 1 then
        List.append top_tags ("" :: tags)
    else if List.length top_tags == List.length tags then
        top_tags
    else
        tags


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

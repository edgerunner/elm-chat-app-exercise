module Chat exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom
import Browser.Events
import Conversation exposing (Conversation, Conversations)
import Dict
import Element exposing (Element, alignTop, centerX, centerY, column, el, fill, height, padding, pointer, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Events as Events
import IdDict exposing (Id)
import Message exposing (Message)
import RemoteData
import Styles exposing (blue, em, gray)
import Task
import User exposing (User, Users)


type Model
    = Model ModelRecord


type alias ModelRecord =
    { users : Users
    , conversations : Conversations
    , focus : Maybe Id
    , width : Int
    }


type Msg
    = FocusConversation Conversation
    | BlurConversation
    | WindowResize Int Int
    | WindowInitialize Browser.Dom.Viewport
    | GotMessages Conversation


init : Users -> Conversations -> ( Model, Cmd Msg )
init users_ conversations =
    ( Model
        { users = users_
        , conversations = conversations
        , focus = Nothing
        , width = 0
        }
    , Task.perform WindowInitialize Browser.Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        only transform =
            ( transform model, Cmd.none )
    in
    case msg of
        FocusConversation conv ->
            conv
                |> Conversation.checkAndLoad GotMessages
                |> Tuple.mapFirst
                    (\c -> replaceAndFocusConversation c model)

        BlurConversation ->
            only blurConversation

        WindowResize width _ ->
            only <| windowResize width

        WindowInitialize window ->
            only <| windowResize (truncate window.viewport.width)

        GotMessages conversation ->
            only <| replaceAndFocusConversation conversation


replaceAndFocusConversation : Conversation -> Model -> Model
replaceAndFocusConversation conversation =
    replaceConversation conversation
        >> focusConversation conversation.id


replaceConversation : Conversation -> Model -> Model
replaceConversation conversation (Model model) =
    Model
        { model
            | conversations =
                model.conversations
                    |> Dict.update conversation.id (\_ -> Just conversation)
        }


focusConversation : Id -> Model -> Model
focusConversation convId (Model model) =
    Model { model | focus = model.conversations |> Dict.get convId |> Maybe.map .id }


blurConversation : Model -> Model
blurConversation (Model model) =
    Model { model | focus = Nothing }


windowResize : Int -> Model -> Model
windowResize width (Model model) =
    Model { model | width = width }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResize


focusedConversation : Model -> Maybe Conversation
focusedConversation (Model model) =
    model.focus
        |> Maybe.map Dict.get
        |> Maybe.andThen ((|>) model.conversations)


focusedMessages : Model -> Maybe (List ( Message, User ))
focusedMessages model =
    model
        |> focusedConversation
        |> Maybe.map .messages
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map
            (IdDict.toList
                >> List.filterMap
                    (\message ->
                        Dict.get message.from (users model)
                            |> Maybe.map (Tuple.pair message)
                    )
            )


users : Model -> Users
users =
    map .users


map : (ModelRecord -> a) -> Model -> a
map map_ (Model model) =
    map_ model


breakpoint : Int
breakpoint =
    em 20


view : Model -> Element Msg
view (Model model) =
    (if model.width > breakpoint then
        fullView

     else
        model.focus
            |> Maybe.map (always conversationView)
            |> Maybe.withDefault listView
    )
        (Model model)


fullView : Model -> Element Msg
fullView model =
    row
        [ width fill
        , height fill
        , spacing (em 1)
        , padding (em 1)
        , Background.color gray
        ]
        [ fullViewBlock shrink (listView model)
        , fullViewBlock fill (conversationView model)
        ]


fullViewBlock : Element.Length -> Element Msg -> Element Msg
fullViewBlock w =
    el
        [ alignTop
        , Background.color Styles.white
        , height fill
        , width w
        ]


listView : Model -> Element Msg
listView model =
    row
        [ width fill
        , height fill
        ]
        [ convList model
        ]


selectionAttributes : Conversation -> Maybe Id -> List (Element.Attribute msg)
selectionAttributes conv focus =
    if
        focus
            |> Maybe.map ((==) conv.id)
            |> Maybe.withDefault False
    then
        [ Background.color blue ]

    else
        []


convList : Model -> Element Msg
convList (Model model) =
    column
        [ width fill
        , height shrink
        , alignTop
        ]
        (List.filterMap
            (\conv ->
                let
                    user =
                        Dict.get conv.with model.users

                    listing justUser =
                        el
                            ([ Events.onClick (FocusConversation conv)
                             , width fill
                             , pointer
                             ]
                                ++ selectionAttributes conv model.focus
                            )
                            (Conversation.listing conv justUser)
                in
                Maybe.map listing user
            )
            (model.conversations |> IdDict.toList)
        )


conversationView : Model -> Element Msg
conversationView model =
    el
        [ width fill
        , height fill
        , Events.onClick BlurConversation
        ]
        (focusedMessages model
            |> Maybe.map messagesView
            |> Maybe.withDefault (blob "Select conversation")
        )


messagesView : List ( Message, User ) -> Element msg
messagesView =
    List.map messageView >> column [ padding (em 1), spacing (em 0.5) ]


messageView : ( Message, User ) -> Element msg
messageView ( message, user ) =
    row [ spacing (em 0.5) ] [ User.avatar 1 user, text message.body ]


blob : String -> Element msg
blob string =
    el [ centerX, centerY ] (text string)

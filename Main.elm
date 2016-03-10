module Main (..) where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)
import Date exposing (fromTime)
import Signal exposing (Signal)
import String


type Action
  = SendMessage
  | Input String
  | NoOp


type StateChange
  = UserAction ( Time, Action )
  | Response ( Time, Bool )


type alias Person =
  { name : String }


type alias Message =
  { content : String
  , sentBy : Person
  , sentOn : Time
  }


type alias History =
  List Message


type alias Model =
  { input : String
  , history : History
  , user : Person
  }



-- helpers


msgTime : Time -> String
msgTime timestamp =
  let
    date =
      fromTime timestamp

    pad n =
      String.padLeft 2 '0' (toString n)
  in
    (pad (Date.hour date)) ++ ":" ++ (pad (Date.minute date))


msgColor : Message -> String
msgColor msg =
  if msg.sentBy == mockUser then
    "beige"
  else
    "pink"


mockResponse : Time -> Message
mockResponse time =
  { content = "hodor"
  , sentBy = { name = "Hodor" }
  , sentOn = time
  }



-- update


update : StateChange -> Model -> Model
update change state =
  case change of
    UserAction ( time, action ) ->
      handleUserAction time action state

    Response ( time, bool ) ->
      { state | history = (mockResponse time) :: state.history }


handleUserAction : Time -> Action -> Model -> Model
handleUserAction time action state =
  case action of
    SendMessage ->
      { state
        | history = (userMessage state time) :: state.history
        , input = ""
      }

    Input text ->
      { state | input = text }

    NoOp ->
      state


userMessage : Model -> Time -> Message
userMessage state time =
  { content = state.input
  , sentBy = state.user
  , sentOn = time
  }



-- view


header : String -> Html
header heading =
  div
    [ style
        [ ( "padding", "0.5rem" )
        , ( "text-align", "center" )
        ]
    ]
    [ h2 [] [ text heading ] ]


messages : Model -> Html
messages model =
  div
    [ style
        [ ( "padding-bottom", "3rem" )
        , ( "overflow", "scroll" )
        ]
    ]
    [ ul
        [ style
            [ ( "list-style", "none" )
            , ( "padding", "0" )
            , ( "margin", "0" )
            ]
        ]
        (model.history
          |> List.reverse
          |> List.map message
        )
    ]


message : Message -> Html
message msg =
  li
    [ style
        [ ( "background", msgColor msg )
        , ( "padding", "0.3rem 0.5rem" )
        , ( "display", "flex" )
        ]
    ]
    [ msgContent msg
    , msgSentOn msg.sentOn
    ]


msgContent : Message -> Html
msgContent msg =
  span
    [ style [ ( "flex", "5" ) ] ]
    [ text (msg.sentBy.name ++ ": " ++ msg.content) ]


msgSentOn : Time -> Html
msgSentOn sentOn =
  span
    [ style
        [ ( "flex", "1" )
        , ( "text-align", "right" )
        ]
    ]
    [ text (msgTime sentOn) ]


inputArea : Model -> Html
inputArea model =
  div
    [ style
        [ ( "display", "flex" )
        , ( "position", "fixed" )
        , ( "bottom", "0" )
        , ( "width", "100%" )
        , ( "border-top", "0.2rem solid #e5e5e5" )
        , ( "height", "2.8rem" )
        ]
    ]
    [ messageInput model.input
    , sendButton
    ]


messageInput : String -> Html
messageInput currentInput =
  input
    [ style
        [ ( "flex", "5" )
        , ( "border", "none" )
        , ( "background", "#eee" )
        , ( "padding", "0.5rem" )
        , ( "font-size", "1.2rem" )
        ]
    , placeholder "Your message..."
    , autofocus True
    , value currentInput
    , on "input" targetValue (\str -> Signal.message actions.address (Input str))
    ]
    []


sendButton : Html
sendButton =
  button
    [ style
        [ ( "flex", "1" )
        , ( "background", "lightblue" )
        , ( "color", "#345B80" )
        , ( "border", "none" )
        , ( "font-size", "1.2rem" )
        , ( "padding", "0.5rem" )
        ]
    , (onClick actions.address SendMessage)
    ]
    [ text ">" ]


mockMessageControl : Html
mockMessageControl =
  button
    [ style
        [ ( "width", "100%" )
        , ( "background", "lightgreen" )
        , ( "border", "none" )
        , ( "font-size", "1rem" )
        , ( "padding", "0.5rem" )
        ]
    , (onClick serverResponses.address True)
    ]
    [ text "Mock response" ]


view : String -> Model -> Html
view heading model =
  div
    []
    [ header heading
    , mockMessageControl
    , messages model
    , inputArea model
    ]


html : Model -> Html
html model =
  view "Convo" model



-- init


mockUser : Person
mockUser =
  { name = "Matias" }


init : Model
init =
  { input = ""
  , history = []
  , user = mockUser
  }


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


serverResponses : Signal.Mailbox Bool
serverResponses =
  Signal.mailbox True


inputSignal : Signal StateChange
inputSignal =
  Signal.merge
    (Signal.map UserAction (Time.timestamp actions.signal))
    (Signal.map Response (Time.timestamp serverResponses.signal))


model : Signal Model
model =
  Signal.foldp update init inputSignal


main : Signal Html
main =
  Signal.map html model

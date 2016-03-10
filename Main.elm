module Main (..) where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)
import Date exposing (fromTime)
import Signal exposing (Signal)


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
  in
    (toString (Date.hour date)) ++ ":" ++ (toString (Date.minute date))


gutter : Attribute
gutter =
  style [ ( "margin-right", "5px" ) ]


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


messages : Model -> Html
messages model =
  div
    []
    [ ul
        []
        (model.history
          |> List.reverse
          |> List.map message
        )
    ]


message : Message -> Html
message msg =
  li
    []
    [ span [ gutter ] [ text (msgTime msg.sentOn) ]
    , span [ gutter ] [ text msg.sentBy.name ]
    , span [] [ text msg.content ]
    ]


inputArea : Model -> Html
inputArea model =
  div
    []
    [ messageInput model.input
    , sendButton
    ]


messageInput : String -> Html
messageInput currentInput =
  input
    [ placeholder "Your message..."
    , autofocus True
    , value currentInput
    , on "input" targetValue (\str -> Signal.message actions.address (Input str))
    ]
    []


sendButton : Html
sendButton =
  button [ onClick actions.address SendMessage ] [ text "Send" ]


mockMessageControl : Html
mockMessageControl =
  button [ onClick serverResponses.address True ] [ text "Mock response" ]


view : String -> Model -> Html
view heading model =
  div
    []
    [ text heading
    , messages model
    , inputArea model
    , mockMessageControl
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

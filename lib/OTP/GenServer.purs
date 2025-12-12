module OTP.GenServer where

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

-- | A reference to a running GenServer process (Erlang pid)
type ServerRef = Unit  -- placeholder, will be Erlang pid at runtime

-- | Result of starting a server
type StartResult = Either String ServerRef

-- | Reply with new state for handle_call
type CallResult state reply = { reply :: reply, newState :: state }

-- | New state for handle_cast
type CastResult state = { newState :: state }

-- | GenServer callbacks
type Callbacks state call cast reply =
  { handleCall :: call -> state -> CallResult state reply
  , handleCast :: cast -> state -> CastResult state
  }

-- | Start a GenServer with initial state and callbacks
startLink :: forall state call cast reply.
  state ->
  Callbacks state call cast reply ->
  StartResult
startLink initialState callbacks = startLinkImpl initialState callbacks

foreign import startLinkImpl :: forall state call cast reply. state -> Callbacks state call cast reply -> StartResult = "call 'Elixir.Nova.GenServer':'start_link'($0, $1)"

-- | Make a synchronous call to the server
call :: forall call reply. ServerRef -> call -> reply
call server msg = callImpl server msg

foreign import callImpl :: forall call reply. ServerRef -> call -> reply = "call 'Elixir.Nova.GenServer':'call'($0, $1)"

-- | Make a synchronous call with timeout
callTimeout :: forall call reply. ServerRef -> call -> Int -> Maybe reply
callTimeout server msg timeout = callTimeoutImpl server msg timeout

foreign import callTimeoutImpl :: forall call reply. ServerRef -> call -> Int -> Maybe reply = "call 'Elixir.Nova.GenServer':'call_timeout'($0, $1, $2)"

-- | Send an asynchronous cast to the server
cast :: forall cast. ServerRef -> cast -> Unit
cast server msg = castImpl server msg

foreign import castImpl :: forall cast. ServerRef -> cast -> Unit = "call 'Elixir.Nova.GenServer':'cast'($0, $1)"

-- | Stop the server
stop :: ServerRef -> Unit
stop server = stopImpl server

foreign import stopImpl :: ServerRef -> Unit = "call 'Elixir.Nova.GenServer':'stop'($0)"

-- | Check if server is alive
isAlive :: ServerRef -> Boolean
isAlive server = isAliveImpl server

foreign import isAliveImpl :: ServerRef -> Boolean = "call 'Elixir.Nova.GenServer':'is_alive'($0)"

module System.Process where

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

-- | Process ID type
type Pid = Unit  -- Opaque at PureScript level, actual pid at runtime

-- | Get current process ID
self :: Pid
self = selfImpl

foreign import selfImpl :: Pid = "call 'erlang':'self'()"

-- | Spawn a new process
spawn :: forall a. (Unit -> a) -> Pid
spawn f = spawnImpl f

foreign import spawnImpl :: forall a. (Unit -> a) -> Pid = "call 'erlang':'spawn'(fun () -> apply $0 ('unit'))"

-- | Send a message to a process
send :: forall a. Pid -> a -> Unit
send pid msg = sendImpl pid msg

foreign import sendImpl :: forall a. Pid -> a -> Unit = "do call 'erlang':'!'($0, $1) 'unit'"

-- | Check if a process is alive
isProcessAlive :: Pid -> Boolean
isProcessAlive pid = isProcessAliveImpl pid

foreign import isProcessAliveImpl :: Pid -> Boolean = "call 'erlang':'is_process_alive'($0)"

-- | Sleep for milliseconds
sleep :: Int -> Unit
sleep ms = sleepImpl ms

foreign import sleepImpl :: Int -> Unit = "do call 'timer':'sleep'($0) 'unit'"

-- | Get environment variable
getEnv :: String -> Maybe String
getEnv name = getEnvImpl name

foreign import getEnvImpl :: String -> Maybe String = "case call 'os':'getenv'($0) of <'false'> when 'true' -> 'Nothing' <Val> when 'true' -> {'Just', Val} end"

-- | Exit with status code
exit :: forall a. Int -> a
exit code = exitImpl code

foreign import exitImpl :: forall a. Int -> a = "call 'erlang':'halt'($0)"

-- | Apply a function dynamically
apply :: forall a. String -> String -> Array a -> a
apply mod func args = applyImpl mod func args

foreign import applyImpl :: forall a. String -> String -> Array a -> a = "call 'erlang':'apply'(call 'erlang':'binary_to_atom'($0, 'utf8'), call 'erlang':'binary_to_atom'($1, 'utf8'), $2)"

-- | Apply to an atom module
applyAtom :: forall a. Unit -> String -> Array a -> a
applyAtom mod func args = applyAtomImpl mod func args

foreign import applyAtomImpl :: forall a. Unit -> String -> Array a -> a = "call 'erlang':'apply'($0, call 'erlang':'binary_to_atom'($1, 'utf8'), $2)"

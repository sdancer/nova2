module OTP.TCP where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | TCP socket type
type Socket = Unit  -- Opaque, actual socket at runtime

-- | Listen socket type
type ListenSocket = Unit  -- Opaque

-- | TCP options
type ListenOptions =
  { port :: Int
  , reuseaddr :: Boolean
  , active :: Boolean
  , packet :: String  -- "line", "raw", etc.
  }

-- | Default listen options
defaultListenOptions :: ListenOptions
defaultListenOptions =
  { port: 9999
  , reuseaddr: true
  , active: false
  , packet: "line"
  }

-- | Listen on a TCP port
listen :: Int -> Either String ListenSocket
listen port = listenImpl port

foreign import listenImpl :: Int -> Either String ListenSocket = "case call 'gen_tcp':'listen'($0, ['binary', {'packet', 'line'}, {'active', 'false'}, {'reuseaddr', 'true'}]) of <{'ok', Socket}> when 'true' -> {'Right', Socket} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"~p\", [Reason]))} end"

-- | Accept a connection
accept :: ListenSocket -> Either String Socket
accept listenSocket = acceptImpl listenSocket

foreign import acceptImpl :: ListenSocket -> Either String Socket = "case call 'gen_tcp':'accept'($0) of <{'ok', Socket}> when 'true' -> {'Right', Socket} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"~p\", [Reason]))} end"

-- | Accept with timeout (milliseconds)
acceptTimeout :: ListenSocket -> Int -> Either String Socket
acceptTimeout listenSocket timeout = acceptTimeoutImpl listenSocket timeout

foreign import acceptTimeoutImpl :: ListenSocket -> Int -> Either String Socket = "case call 'gen_tcp':'accept'($0, $1) of <{'ok', Socket}> when 'true' -> {'Right', Socket} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"~p\", [Reason]))} end"

-- | Receive data from socket
recv :: Socket -> Either String String
recv socket = recvImpl socket

foreign import recvImpl :: Socket -> Either String String = "case call 'gen_tcp':'recv'($0, 0) of <{'ok', Data}> when 'true' -> {'Right', Data} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"~p\", [Reason]))} end"

-- | Receive data with timeout
recvTimeout :: Socket -> Int -> Either String String
recvTimeout socket timeout = recvTimeoutImpl socket timeout

foreign import recvTimeoutImpl :: Socket -> Int -> Either String String = "case call 'gen_tcp':'recv'($0, 0, $1) of <{'ok', Data}> when 'true' -> {'Right', Data} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"~p\", [Reason]))} end"

-- | Send data to socket
send :: Socket -> String -> Either String Unit
send socket data_ = sendImpl socket data_

foreign import sendImpl :: Socket -> String -> Either String Unit = "case call 'gen_tcp':'send'($0, $1) of <'ok'> when 'true' -> {'Right', 'unit'} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"~p\", [Reason]))} end"

-- | Close a socket
close :: Socket -> Unit
close socket = closeImpl socket

foreign import closeImpl :: Socket -> Unit = "do call 'gen_tcp':'close'($0) 'unit'"

-- | Close listen socket
closeListen :: ListenSocket -> Unit
closeListen socket = closeListenImpl socket

foreign import closeListenImpl :: ListenSocket -> Unit = "do call 'gen_tcp':'close'($0) 'unit'"

-- | Set socket options
setOpts :: Socket -> Boolean -> Either String Unit
setOpts socket active = setOptsImpl socket active

foreign import setOptsImpl :: Socket -> Boolean -> Either String Unit = "case call 'inet':'setopts'($0, [{'active', $1}]) of <'ok'> when 'true' -> {'Right', 'unit'} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"~p\", [Reason]))} end"

-- | Control socket (transfer ownership)
controlling_process :: Socket -> Unit -> Either String Unit
controlling_process socket pid = controllingProcessImpl socket pid

foreign import controllingProcessImpl :: Socket -> Unit -> Either String Unit = "case call 'gen_tcp':'controlling_process'($0, $1) of <'ok'> when 'true' -> {'Right', 'unit'} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'iolist_to_binary'(call 'io_lib':'format'(\"~p\", [Reason]))} end"

-- | Transport layer for MCP (STDIO and TCP)
module MCP.Transport where

import Prelude
import Data.Either (Either(..))

-- | Socket type (opaque - represented as Erlang term)
type Socket = Unit

-- | Transport type
data Transport
  = StdioTransport
  | TcpTransport { listenSocket :: Socket, clientSocket :: Socket, port :: Int }

-- | Read a line from STDIO
stdioReadLine :: Unit -> Either String String
stdioReadLine _ = stdioReadLineImpl unit

foreign import stdioReadLineImpl :: Unit -> Either String String
  = "case call 'io':'get_line'('') of <'eof'> when 'true' -> {'Left', [101,111,102]} <{'error', _R}> when 'true' -> {'Left', [101,114,114,111,114]} <Line> when 'true' -> {'Right', call 'string':'trim'(Line)} end"

-- | Write a line to STDOUT
stdioWriteLine :: String -> Unit
stdioWriteLine s = stdioWriteLineImpl s

foreign import stdioWriteLineImpl :: String -> Unit
  = "let <_> = call 'io':'put_chars'($0) in let <_> = call 'io':'nl'() in 'unit'"

-- | Write to STDERR (for logging)
stderrWrite :: String -> Unit
stderrWrite s = stderrWriteImpl s

foreign import stderrWriteImpl :: String -> Unit
  = "let <_> = call 'io':'put_chars'('standard_error', $0) in let <_> = call 'io':'nl'('standard_error') in 'unit'"

-- | Listen on a TCP port
tcpListen :: Int -> Either String Socket
tcpListen port = tcpListenImpl port

foreign import tcpListenImpl :: Int -> Either String Socket
  = "case call 'gen_tcp':'listen'($0, ['binary', {'packet', 'line'}, {'active', 'false'}, {'reuseaddr', 'true'}]) of <{'ok', Socket}> when 'true' -> {'Right', Socket} <{'error', _R}> when 'true' -> {'Left', [101,114,114,111,114]} end"

-- | Accept a TCP connection
tcpAccept :: Socket -> Either String Socket
tcpAccept sock = tcpAcceptImpl sock

foreign import tcpAcceptImpl :: Socket -> Either String Socket
  = "case call 'gen_tcp':'accept'($0) of <{'ok', ClientSocket}> when 'true' -> {'Right', ClientSocket} <{'error', _R}> when 'true' -> {'Left', [101,114,114,111,114]} end"

-- | Read a line from TCP socket
tcpReadLine :: Socket -> Either String String
tcpReadLine sock = tcpReadLineImpl sock

foreign import tcpReadLineImpl :: Socket -> Either String String
  = "case call 'gen_tcp':'recv'($0, 0) of <{'ok', Data}> when 'true' -> {'Right', call 'erlang':'binary_to_list'(call 'string':'trim'(Data))} <{'error', 'closed'}> when 'true' -> {'Left', [99,108,111,115,101,100]} <{'error', _R}> when 'true' -> {'Left', [101,114,114,111,114]} end"

-- | Write a line to TCP socket
tcpWriteLine :: Socket -> String -> Either String Unit
tcpWriteLine sock msg = tcpWriteLineImpl sock msg

foreign import tcpWriteLineImpl :: Socket -> String -> Either String Unit
  = "let <NL> = call 'erlang':'list_to_binary'([10]) in case call 'gen_tcp':'send'($0, [call 'erlang':'list_to_binary'($1), NL]) of <'ok'> when 'true' -> {'Right', 'unit'} <{'error', _R}> when 'true' -> {'Left', [101,114,114,111,114]} end"

-- | Close a TCP socket
tcpClose :: Socket -> Unit
tcpClose sock = tcpCloseImpl sock

foreign import tcpCloseImpl :: Socket -> Unit
  = "let <_> = call 'gen_tcp':'close'($0) in 'unit'"

-- | Initialize STDIO transport
initStdio :: Transport
initStdio = StdioTransport

-- | Initialize TCP transport on a port
initTcp :: Int -> Either String Transport
initTcp port =
  case tcpListen port of
    Left err -> Left ("Failed to listen: " <> err)
    Right listenSock ->
      let _ = stderrWrite ("Listening on port " <> show port)
      in case tcpAccept listenSock of
        Left err -> Left ("Failed to accept: " <> err)
        Right clientSock ->
          let _ = stderrWrite "Client connected"
          in Right (TcpTransport { listenSocket: listenSock, clientSocket: clientSock, port: port })

-- | Read a line from transport
readLine :: Transport -> Either String String
readLine StdioTransport = stdioReadLine unit
readLine (TcpTransport r) = tcpReadLine r.clientSocket

-- | Write a line to transport
writeLine :: Transport -> String -> Either String Unit
writeLine StdioTransport msg = Right (stdioWriteLine msg)
writeLine (TcpTransport r) msg = tcpWriteLine r.clientSocket msg

-- | Close transport
closeTransport :: Transport -> Unit
closeTransport StdioTransport = unit
closeTransport (TcpTransport r) =
  let _ = tcpClose r.clientSocket
  in tcpClose r.listenSocket

-- | Log a message (always goes to stderr)
logInfo :: String -> Unit
logInfo msg = stderrWrite ("[INFO] " <> msg)

logError :: String -> Unit
logError msg = stderrWrite ("[ERROR] " <> msg)

logDebug :: String -> Unit
logDebug msg = stderrWrite ("[DEBUG] " <> msg)

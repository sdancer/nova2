-- | Transport layer for MCP (STDIO and TCP)
module MCP.Transport where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | Opaque socket type
foreign import data Socket :: Type

-- | Transport type
data Transport
  = StdioTransport
  | TcpTransport { listenSocket :: Socket, clientSocket :: Socket, port :: Int }

-- | Read a line from STDIO
foreign import stdioReadLine :: Unit -> Either String String
  = "case call 'io':'get_line'('') of
       <'eof'> when 'true' -> {'Left', \"EOF\"}
       <{'error', Reason}> when 'true' ->
         {'Left', call 'erlang':'binary_to_list'(call 'erlang':'term_to_binary'(Reason))}
       <Line> when 'true' ->
         {'Right', call 'string':'trim'(Line)}
     end"

-- | Write a line to STDOUT
foreign import stdioWriteLine :: String -> Unit
  = "do call 'io':'put_chars'($0)
        call 'io':'nl'()
        'unit'"

-- | Write to STDERR (for logging)
foreign import stderrWrite :: String -> Unit
  = "do call 'io':'put_chars'('standard_error', $0)
        call 'io':'nl'('standard_error')
        'unit'"

-- | Listen on a TCP port
foreign import tcpListen :: Int -> Either String Socket
  = "case call 'gen_tcp':'listen'($0, ['binary', {'packet', 'line'}, {'active', 'false'}, {'reuseaddr', 'true'}]) of
       <{'ok', Socket}> when 'true' -> {'Right', Socket}
       <{'error', Reason}> when 'true' ->
         {'Left', call 'erlang':'binary_to_list'(call 'erlang':'atom_to_binary'(Reason, 'utf8'))}
     end"

-- | Accept a TCP connection
foreign import tcpAccept :: Socket -> Either String Socket
  = "case call 'gen_tcp':'accept'($0) of
       <{'ok', ClientSocket}> when 'true' -> {'Right', ClientSocket}
       <{'error', Reason}> when 'true' ->
         {'Left', call 'erlang':'binary_to_list'(call 'erlang':'atom_to_binary'(Reason, 'utf8'))}
     end"

-- | Read a line from TCP socket
foreign import tcpReadLine :: Socket -> Either String String
  = "case call 'gen_tcp':'recv'($0, 0) of
       <{'ok', Data}> when 'true' ->
         {'Right', call 'erlang':'binary_to_list'(call 'string':'trim'(Data))}
       <{'error', 'closed'}> when 'true' -> {'Left', \"Connection closed\"}
       <{'error', Reason}> when 'true' ->
         {'Left', call 'erlang':'binary_to_list'(call 'erlang':'atom_to_binary'(Reason, 'utf8'))}
     end"

-- | Write a line to TCP socket
foreign import tcpWriteLine :: Socket -> String -> Either String Unit
  = "case call 'gen_tcp':'send'($0, [call 'erlang':'list_to_binary'($1), <<\"\\n\">>]) of
       <'ok'> when 'true' -> {'Right', 'unit'}
       <{'error', Reason}> when 'true' ->
         {'Left', call 'erlang':'binary_to_list'(call 'erlang':'atom_to_binary'(Reason, 'utf8'))}
     end"

-- | Close a TCP socket
foreign import tcpClose :: Socket -> Unit
  = "do call 'gen_tcp':'close'($0) 'unit'"

-- | Initialize STDIO transport
initStdio :: Transport
initStdio = StdioTransport

-- | Initialize TCP transport on a port
initTcp :: Int -> Either String Transport
initTcp port =
  case tcpListen port of
    Left err -> Left ("Failed to listen on port " <> show port <> ": " <> err)
    Right listenSock -> do
      let _ = stderrWrite ("MCP server listening on port " <> show port)
      let _ = stderrWrite "Waiting for client connection..."
      case tcpAccept listenSock of
        Left err -> Left ("Failed to accept connection: " <> err)
        Right clientSock -> do
          let _ = stderrWrite "Client connected"
          Right (TcpTransport { listenSocket: listenSock, clientSocket: clientSock, port: port })

-- | Read a line from transport
readLine :: Transport -> Either String String
readLine StdioTransport = stdioReadLine unit
readLine (TcpTransport { clientSocket }) = tcpReadLine clientSocket

-- | Write a line to transport
writeLine :: Transport -> String -> Either String Unit
writeLine StdioTransport msg = Right (stdioWriteLine msg)
writeLine (TcpTransport { clientSocket }) msg = tcpWriteLine clientSocket msg

-- | Close transport
closeTransport :: Transport -> Unit
closeTransport StdioTransport = unit
closeTransport (TcpTransport { listenSocket, clientSocket }) = do
  let _ = tcpClose clientSocket
  tcpClose listenSocket

-- | Log a message (always goes to stderr)
logInfo :: String -> Unit
logInfo msg = stderrWrite ("[INFO] " <> msg)

logError :: String -> Unit
logError msg = stderrWrite ("[ERROR] " <> msg)

logDebug :: String -> Unit
logDebug msg = stderrWrite ("[DEBUG] " <> msg)

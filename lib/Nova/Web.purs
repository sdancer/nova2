-- | Nova Web UI - Simple backend-rendered HTML interface
module Nova.Web where

import Prelude
import Data.Either (Either(..))
import Nova.Eval as Eval

-- | Start the web server on a given port
-- Uses Nova.HTTPServer Elixir wrapper
startServer :: Int -> Unit
startServer port = startServerImpl port

foreign import startServerImpl :: Int -> Unit
  = "call 'Elixir.Nova.HTTPServer':'start'($0, 'Nova.Web')"

-- | HTTP handler - called by Nova.HTTPServer
-- handle :: String -> String -> String -> {Int, String, String}
handle :: String -> String -> String -> { status :: Int, contentType :: String, body :: String }
handle method path body =
  if method == "GET"
  then handleGet path
  else if method == "POST" && path == "/api/eval"
  then handlePost body
  else { status: 404, contentType: "text/plain", body: "Not Found" }

-- | Handle GET requests
handleGet :: String -> { status :: Int, contentType :: String, body :: String }
handleGet path =
  if path == "/" || path == "/index.html"
  then { status: 200, contentType: "text/html", body: indexPage }
  else if path == "/eval"
  then { status: 200, contentType: "text/html", body: evalPage }
  else { status: 404, contentType: "text/html", body: notFoundPage }

-- | Handle POST /api/eval
handlePost :: String -> { status :: Int, contentType :: String, body :: String }
handlePost body =
  let action = extractJsonField body "action"
      code = extractJsonField body "code"
      result = if action == "eval_expr"
               then case Eval.eval code of
                 Right r -> "{\"result\":\"" <> escapeJson r <> "\"}"
                 Left e -> "{\"error\":\"" <> escapeJson e <> "\"}"
               else if action == "compile_to_core"
               then case Eval.compileToCore code of
                 Right r -> "{\"result\":\"" <> escapeJson r <> "\"}"
                 Left e -> "{\"error\":\"" <> escapeJson e <> "\"}"
               else "{\"error\":\"Unknown action\"}"
  in { status: 200, contentType: "application/json", body: result }

-- | Extract field from JSON
foreign import extractJsonField :: String -> String -> String
  = "let <Bin> = call 'erlang':'list_to_binary'($0) in case catch call 'json':'decode'(Bin) of <{'EXIT', _}> when 'true' -> [] <Map> when call 'erlang':'is_map'(Map) -> let <Key> = call 'erlang':'list_to_binary'($1) in case call 'maps':'find'(Key, Map) of <{'ok', V}> when call 'erlang':'is_binary'(V) -> call 'erlang':'binary_to_list'(V) <_> when 'true' -> [] end <_> when 'true' -> [] end"

-- | Escape JSON string
foreign import escapeJson :: String -> String
  = "call 'lists':'flatten'(call 'lists':'map'(fun (C) -> case C of <34> when 'true' -> [92,34] <92> when 'true' -> [92,92] <10> when 'true' -> [92,110] <13> when 'true' -> [92,114] <9> when 'true' -> [92,116] <_> when 'true' -> [C] end, $0))"

-- | CSS styles
styles :: String
styles = "<style>*{box-sizing:border-box;margin:0;padding:0}body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,sans-serif;background:#1a1a2e;color:#eee;min-height:100vh;padding:2rem}.hero{text-align:center;padding:3rem 0}.hero h1{font-size:3rem;color:#00d4ff;margin-bottom:.5rem}.hero p{font-size:1.2rem;color:#888}.nav-cards{display:flex;gap:1rem;justify-content:center;flex-wrap:wrap;margin-top:2rem}.card{background:#252540;padding:1.5rem 2rem;border-radius:8px;text-decoration:none;color:#eee;transition:transform .2s,background .2s}.card:hover{transform:translateY(-2px);background:#303050}.card h2{color:#00d4ff;margin-bottom:.5rem}.card p{color:#888;font-size:.9rem}h1{color:#00d4ff;margin-bottom:1rem}.repl-container{display:grid;grid-template-columns:1fr 1fr;gap:1rem;margin-top:1rem}.input-section,.output-section{background:#252540;padding:1rem;border-radius:8px}label{display:block;margin-bottom:.5rem;color:#888}textarea{width:100%;background:#1a1a2e;color:#eee;border:1px solid #444;border-radius:4px;padding:.75rem;font-family:Monaco,Menlo,monospace;font-size:14px;resize:vertical}textarea:focus{outline:none;border-color:#00d4ff}.buttons{margin-top:1rem;display:flex;gap:.5rem;flex-wrap:wrap}button{background:#00d4ff;color:#1a1a2e;border:none;padding:.5rem 1rem;border-radius:4px;cursor:pointer;font-weight:600;transition:background .2s}button:hover{background:#00b8e6}pre{background:#1a1a2e;padding:1rem;border-radius:4px;overflow-x:auto;font-family:Monaco,Menlo,monospace;font-size:14px;white-space:pre-wrap;min-height:200px}</style>"

-- | HTML wrapper
htmlPage :: String -> String -> String
htmlPage title content =
  "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>" <> title <> "</title>" <> styles <> "</head><body>" <> content <> "</body></html>"

-- | Index page
indexPage :: String
indexPage = htmlPage "Nova Lang" "<div class=\"hero\"><h1>Nova Lang</h1><p>A PureScript dialect that compiles to Core Erlang</p></div><div class=\"nav-cards\"><a href=\"/eval\" class=\"card\"><h2>REPL</h2><p>Evaluate Nova expressions interactively</p></a></div>"

-- | REPL page
evalPage :: String
evalPage = htmlPage "Nova REPL" "<h1>Nova REPL</h1><div class=\"repl-container\"><div class=\"input-section\"><label>Enter Nova code:</label><textarea id=\"code\" rows=\"10\" placeholder=\"1 + 2 * 3\"></textarea><div class=\"buttons\"><button onclick=\"evalCode()\">Evaluate</button><button onclick=\"showCore()\">Show Core Erlang</button></div></div><div class=\"output-section\"><label>Result:</label><pre id=\"result\">-- Output will appear here</pre></div></div><script>async function evalCode(){const c=document.getElementById('code').value;const r=await fetch('/api/eval',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({action:'eval_expr',code:c})});const d=await r.json();document.getElementById('result').textContent=d.result||d.error}async function showCore(){const c=document.getElementById('code').value;const r=await fetch('/api/eval',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({action:'compile_to_core',code:c})});const d=await r.json();document.getElementById('result').textContent=d.result||d.error}</script>"

-- | 404 page
notFoundPage :: String
notFoundPage = htmlPage "Not Found" "<h1>404 - Not Found</h1><p><a href=\"/\">Go home</a></p>"

-- | Nova Web UI - Simple backend-rendered HTML interface
module Nova.Web where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as Array
import Data.String as String
import Data.Json as Json
import OTP.PersistentTerm as PT
import Nova.Eval as Eval
import Nova.NamespaceService as NS

-- | Global state key
stateKey :: String
stateKey = "nova_web_state"

-- | Initialize the service if not already initialized
initServiceIfNeeded :: Unit -> Unit
initServiceIfNeeded _ =
  case PT.get stateKey of
    Nothing ->
      case NS.init unit of
        Right st -> PT.put stateKey st
        Left _ -> unit
    Just _ -> unit

-- | Get the current service state (unsafe - assumes initialized)
foreign import getState :: Unit -> NS.ServiceState
  = "call 'persistent_term':'get'('nova_web_state')"

-- | List all namespace names (uses FFI to avoid type checker issues with ServiceState)
foreign import listNamespaces :: Unit -> Array String
  = "let <_> = apply 'initServiceIfNeeded'/1('unit') in let <St> = apply 'getState'/1('unit') in call 'Nova.NamespaceService':'listNamespaces'(St)"

-- | Check if namespace exists
foreign import namespaceExists :: String -> Boolean
  = "let <_> = apply 'initServiceIfNeeded'/1('unit') in let <St> = apply 'getState'/1('unit') in call 'Nova.NamespaceService':'namespaceExists'(St, $0)"

-- | Create a namespace
foreign import createNamespace :: String -> Either String Unit
  = "let <_> = apply 'initServiceIfNeeded'/1('unit') in let <St> = apply 'getState'/1('unit') in call 'Nova.NamespaceService':'createNamespace'(St, $0)"

-- | Delete a namespace
foreign import deleteNamespace :: String -> Either String Unit
  = "let <_> = apply 'initServiceIfNeeded'/1('unit') in let <St> = apply 'getState'/1('unit') in call 'Nova.NamespaceService':'deleteNamespace'(St, $0)"

-- | Get declarations in a namespace
foreign import getNamespaceDecls :: String -> Array NS.ManagedDecl
  = "let <_> = apply 'initServiceIfNeeded'/1('unit') in let <St> = apply 'getState'/1('unit') in call 'Nova.NamespaceService':'getNamespaceDecls'(St, $0)"

-- | Add a declaration
foreign import addDecl :: String -> String -> String -> NS.DeclKind -> Either String String
  = "let <_> = apply 'initServiceIfNeeded'/1('unit') in let <St> = apply 'getState'/1('unit') in call 'Nova.NamespaceService':'addDecl'(St, $0, $1, $2, $3)"

-- | Start the web server on a given port
-- Uses Nova.HTTPServer Elixir wrapper
startServer :: Int -> Unit
startServer port =
  let _w = initServiceIfNeeded unit
  in startServerImpl port

foreign import startServerImpl :: Int -> Unit
  = "call 'Elixir.Nova.HTTPServer':'start'($0, 'Nova.Web')"

-- | HTTP handler - called by Nova.HTTPServer
handle :: String -> String -> String -> { status :: Int, contentType :: String, body :: String }
handle method path body =
  if method == "GET"
  then handleGet path
  else if method == "POST"
  then handlePost path body
  else { status: 404, contentType: "text/plain", body: "Not Found" }

-- | Handle GET requests
handleGet :: String -> { status :: Int, contentType :: String, body :: String }
handleGet path =
  if path == "/" || path == "/index.html"
  then { status: 200, contentType: "text/html", body: indexPage }
  else if path == "/eval"
  then { status: 200, contentType: "text/html", body: evalPage }
  else if path == "/namespaces"
  then { status: 200, contentType: "text/html", body: namespacesPage unit }
  else if String.take 11 path == "/namespace/"
  then
    let nsName = String.drop 11 path
    in { status: 200, contentType: "text/html", body: namespaceDetailPage nsName }
  else { status: 404, contentType: "text/html", body: notFoundPage }

-- | Handle POST requests
handlePost :: String -> String -> { status :: Int, contentType :: String, body :: String }
handlePost path body =
  if path == "/api/eval"
  then handleEvalPost body
  else if path == "/api/namespace/create"
  then handleCreateNamespace body
  else if path == "/api/namespace/delete"
  then handleDeleteNamespace body
  else if path == "/api/decl/add"
  then handleAddDecl body
  else { status: 404, contentType: "application/json", body: "{\"error\":\"Not Found\"}" }

-- | Extract field from JSON using Json module
extractJsonField :: String -> String -> String
extractJsonField jsonStr field =
  case Json.decode jsonStr of
    Nothing -> ""
    Just obj -> fromMaybe "" (Json.getString obj field)

-- | Handle POST /api/eval
handleEvalPost :: String -> { status :: Int, contentType :: String, body :: String }
handleEvalPost body =
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

-- | Handle POST /api/namespace/create
handleCreateNamespace :: String -> { status :: Int, contentType :: String, body :: String }
handleCreateNamespace body =
  let name = extractJsonField body "name"
  in if name == ""
     then { status: 400, contentType: "application/json", body: "{\"error\":\"Name required\"}" }
     else case createNamespace name of
       Right _ -> { status: 200, contentType: "application/json", body: "{\"success\":true}" }
       Left err -> { status: 400, contentType: "application/json", body: "{\"error\":\"" <> escapeJson err <> "\"}" }

-- | Handle POST /api/namespace/delete
handleDeleteNamespace :: String -> { status :: Int, contentType :: String, body :: String }
handleDeleteNamespace body =
  let name = extractJsonField body "name"
  in case deleteNamespace name of
       Right _ -> { status: 200, contentType: "application/json", body: "{\"success\":true}" }
       Left err -> { status: 400, contentType: "application/json", body: "{\"error\":\"" <> escapeJson err <> "\"}" }

-- | Handle POST /api/decl/add
handleAddDecl :: String -> { status :: Int, contentType :: String, body :: String }
handleAddDecl body =
  let namespace = extractJsonField body "namespace"
      name = extractJsonField body "name"
      source = extractJsonField body "source"
      kindStr = extractJsonField body "kind"
      kind = if kindStr == "datatype" then NS.DatatypeDecl
             else if kindStr == "typealias" then NS.TypeAliasDecl
             else if kindStr == "foreign" then NS.ForeignDecl
             else NS.FunctionDecl
  in case addDecl namespace name source kind of
       Right declId -> { status: 200, contentType: "application/json", body: "{\"declId\":\"" <> escapeJson declId <> "\"}" }
       Left err -> { status: 400, contentType: "application/json", body: "{\"error\":\"" <> escapeJson err <> "\"}" }

-- | Escape JSON string
foreign import escapeJson :: String -> String
  = "call 'erlang':'iolist_to_binary'(call 'lists':'map'(fun (C) -> case C of <34> when 'true' -> [92,34] <92> when 'true' -> [92,92] <10> when 'true' -> [92,110] <13> when 'true' -> [92,114] <9> when 'true' -> [92,116] <_> when 'true' -> [C] end, call 'unicode':'characters_to_list'($0)))"

-- | CSS styles
styles :: String
styles = "<style>*{box-sizing:border-box;margin:0;padding:0}body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,sans-serif;background:#1a1a2e;color:#eee;min-height:100vh;padding:2rem}.hero{text-align:center;padding:3rem 0}.hero h1{font-size:3rem;color:#00d4ff;margin-bottom:.5rem}.hero p{font-size:1.2rem;color:#888}.nav-cards{display:flex;gap:1rem;justify-content:center;flex-wrap:wrap;margin-top:2rem}.card{background:#252540;padding:1.5rem 2rem;border-radius:8px;text-decoration:none;color:#eee;transition:transform .2s,background .2s;border:1px solid #333}.card:hover{transform:translateY(-2px);background:#303050;border-color:#00d4ff}.card h2{color:#00d4ff;margin-bottom:.5rem}.card p{color:#888;font-size:.9rem}h1{color:#00d4ff;margin-bottom:1rem}h2{color:#00d4ff;margin:1.5rem 0 1rem}.repl-container{display:grid;grid-template-columns:1fr 1fr;gap:1rem;margin-top:1rem}.input-section,.output-section{background:#252540;padding:1rem;border-radius:8px}label{display:block;margin-bottom:.5rem;color:#888}textarea,input[type=text]{width:100%;background:#1a1a2e;color:#eee;border:1px solid #444;border-radius:4px;padding:.75rem;font-family:Monaco,Menlo,monospace;font-size:14px}textarea{resize:vertical}textarea:focus,input:focus{outline:none;border-color:#00d4ff}.buttons{margin-top:1rem;display:flex;gap:.5rem;flex-wrap:wrap}button{background:#00d4ff;color:#1a1a2e;border:none;padding:.5rem 1rem;border-radius:4px;cursor:pointer;font-weight:600;transition:background .2s}button:hover{background:#00b8e6}button.danger{background:#ff4757}button.danger:hover{background:#ff3344}pre{background:#1a1a2e;padding:1rem;border-radius:4px;overflow-x:auto;font-family:Monaco,Menlo,monospace;font-size:14px;white-space:pre-wrap;min-height:100px}.ns-list{display:grid;gap:1rem;margin-top:1rem}.ns-item{background:#252540;padding:1rem;border-radius:8px;display:flex;justify-content:space-between;align-items:center;border:1px solid #333}.ns-item:hover{border-color:#444}.ns-item a{color:#00d4ff;text-decoration:none;font-size:1.1rem}.ns-item a:hover{text-decoration:underline}.ns-item .actions{display:flex;gap:.5rem}.decl-list{margin-top:1rem}.decl-item{background:#252540;padding:1rem;border-radius:8px;margin-bottom:.5rem;border-left:3px solid #00d4ff}.decl-item.invalid{border-left-color:#ff4757}.decl-item.stale{border-left-color:#ffa502}.decl-item h3{color:#eee;margin-bottom:.5rem;font-size:1rem}.decl-item .meta{color:#888;font-size:.85rem;margin-bottom:.5rem}.decl-item pre{margin-top:.5rem;font-size:12px;max-height:150px;overflow:auto}.form-group{margin-bottom:1rem}.back-link{color:#888;text-decoration:none;display:inline-block;margin-bottom:1rem}.back-link:hover{color:#00d4ff}.badge{display:inline-block;padding:.2rem .5rem;border-radius:3px;font-size:.75rem;margin-left:.5rem}.badge.fresh{background:#00d4ff;color:#1a1a2e}.badge.valid{background:#2ed573;color:#1a1a2e}.badge.invalid{background:#ff4757;color:#fff}.badge.stale{background:#ffa502;color:#1a1a2e}.empty{color:#888;text-align:center;padding:2rem}</style>"

-- | HTML wrapper
htmlPage :: String -> String -> String
htmlPage title content =
  "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>" <> title <> "</title>" <> styles <> "</head><body>" <> content <> "</body></html>"

-- | Index page
indexPage :: String
indexPage = htmlPage "Nova Lang" "<div class=\"hero\"><h1>Nova Lang</h1><p>A PureScript dialect that compiles to Core Erlang</p></div><div class=\"nav-cards\"><a href=\"/namespaces\" class=\"card\"><h2>Namespaces</h2><p>Browse and manage code namespaces</p></a><a href=\"/eval\" class=\"card\"><h2>REPL</h2><p>Evaluate Nova expressions interactively</p></a></div>"

-- | Namespaces page
namespacesPage :: Unit -> String
namespacesPage _u =
  let namespaces = listNamespaces unit
      nsListHtml = if Array.null namespaces
                   then "<div class=\"empty\">No namespaces yet. Create one to get started.</div>"
                   else String.joinWith "" (Array.map renderNsItem namespaces)
  in htmlPage "Namespaces - Nova" ("<a href=\"/\" class=\"back-link\">&larr; Home</a><h1>Namespaces</h1><div class=\"form-group\"><input type=\"text\" id=\"nsName\" placeholder=\"Namespace name (e.g. MyModule)\"><button onclick=\"createNs()\" style=\"margin-left:.5rem\">Create Namespace</button></div><div class=\"ns-list\">" <> nsListHtml <> "</div><script>async function createNs(){const n=document.getElementById('nsName').value;if(!n)return alert('Enter a name');const r=await fetch('/api/namespace/create',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({name:n})});const d=await r.json();if(d.error)alert(d.error);else location.reload()}async function deleteNs(n){if(!confirm('Delete namespace '+n+'?'))return;const r=await fetch('/api/namespace/delete',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({name:n})});const d=await r.json();if(d.error)alert(d.error);else location.reload()}</script>")

renderNsItem :: String -> String
renderNsItem name =
  "<div class=\"ns-item\"><a href=\"/namespace/" <> name <> "\">" <> name <> "</a><div class=\"actions\"><button class=\"danger\" onclick=\"deleteNs('" <> name <> "')\">Delete</button></div></div>"

-- | Namespace detail page
namespaceDetailPage :: String -> String
namespaceDetailPage nsName =
  if not (namespaceExists nsName)
  then htmlPage "Not Found - Nova" "<a href=\"/namespaces\" class=\"back-link\">&larr; Namespaces</a><h1>Namespace Not Found</h1><p>The namespace \"" <> nsName <> "\" does not exist.</p>"
  else
    let decls = getNamespaceDecls nsName
        declsHtml = if Array.null decls
                    then "<div class=\"empty\">No declarations in this namespace.</div>"
                    else String.joinWith "" (Array.map renderDeclItem decls)
    in htmlPage (nsName <> " - Nova") ("<a href=\"/namespaces\" class=\"back-link\">&larr; Namespaces</a><h1>" <> nsName <> "</h1><h2>Add Declaration</h2><div class=\"form-group\"><input type=\"text\" id=\"declName\" placeholder=\"Declaration name\" style=\"margin-bottom:.5rem\"><br><select id=\"declKind\" style=\"background:#1a1a2e;color:#eee;border:1px solid #444;padding:.5rem;border-radius:4px\"><option value=\"function\">Function</option><option value=\"datatype\">Data Type</option><option value=\"typealias\">Type Alias</option><option value=\"foreign\">Foreign</option></select></div><div class=\"form-group\"><textarea id=\"declSource\" rows=\"4\" placeholder=\"add :: Int -> Int -> Int\\nadd x y = x + y\"></textarea></div><button onclick=\"addDecl()\">Add Declaration</button><h2>Declarations</h2><div class=\"decl-list\">" <> declsHtml <> "</div><script>async function addDecl(){const n=document.getElementById('declName').value;const s=document.getElementById('declSource').value;const k=document.getElementById('declKind').value;if(!n||!s)return alert('Name and source required');const r=await fetch('/api/decl/add',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({namespace:'" <> nsName <> "',name:n,source:s,kind:k})});const d=await r.json();if(d.error)alert(d.error);else location.reload()}</script>")

renderDeclItem :: NS.ManagedDecl -> String
renderDeclItem decl =
  let statusClass = case decl.status of
        NS.Fresh -> "fresh"
        NS.Valid -> "valid"
        NS.Invalid -> "invalid"
        NS.Stale -> "stale"
      statusBadge = "<span class=\"badge " <> statusClass <> "\">" <> statusClass <> "</span>"
      kindStr = case decl.kind of
        NS.FunctionDecl -> "function"
        NS.DatatypeDecl -> "data"
        NS.TypeAliasDecl -> "type"
        NS.ForeignDecl -> "foreign"
      typeInfo = case decl.inferredType of
        Nothing -> ""
        Just t -> " :: " <> t
  in "<div class=\"decl-item " <> statusClass <> "\"><h3>" <> decl.name <> typeInfo <> statusBadge <> "</h3><div class=\"meta\">Kind: " <> kindStr <> " | ID: " <> decl.declId <> "</div><pre>" <> decl.sourceText <> "</pre></div>"

-- | REPL page
evalPage :: String
evalPage = htmlPage "Nova REPL" "<a href=\"/\" class=\"back-link\">&larr; Home</a><h1>Nova REPL</h1><div class=\"repl-container\"><div class=\"input-section\"><label>Enter Nova code:</label><textarea id=\"code\" rows=\"10\" placeholder=\"1 + 2 * 3\"></textarea><div class=\"buttons\"><button onclick=\"evalCode()\">Evaluate</button><button onclick=\"showCore()\">Show Core Erlang</button></div></div><div class=\"output-section\"><label>Result:</label><pre id=\"result\">-- Output will appear here</pre></div></div><script>async function evalCode(){const c=document.getElementById('code').value;const r=await fetch('/api/eval',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({action:'eval_expr',code:c})});const d=await r.json();document.getElementById('result').textContent=d.result||d.error}async function showCore(){const c=document.getElementById('code').value;const r=await fetch('/api/eval',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({action:'compile_to_core',code:c})});const d=await r.json();document.getElementById('result').textContent=d.result||d.error}</script>"

-- | 404 page
notFoundPage :: String
notFoundPage = htmlPage "Not Found" "<h1>404 - Not Found</h1><p><a href=\"/\">Go home</a></p>"

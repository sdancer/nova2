module System.File where

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array (concatMap, filter)
import Data.String (endsWith)

-- | Read a file's contents
readFile :: String -> Either String String
readFile path = readFileImpl path

foreign import readFileImpl :: String -> Either String String = "case call 'file':'read_file'($0) of <{'ok', Content}> when 'true' -> {'Right', Content} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'atom_to_binary'(Reason, 'utf8')} end"

-- | Write contents to a file
writeFile :: String -> String -> Either String Unit
writeFile path content = writeFileImpl path content

foreign import writeFileImpl :: String -> String -> Either String Unit = "case call 'file':'write_file'($0, $1) of <'ok'> when 'true' -> {'Right', 'unit'} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'atom_to_binary'(Reason, 'utf8')} end"

-- | Append to a file
appendFile :: String -> String -> Either String Unit
appendFile path content = appendFileImpl path content

foreign import appendFileImpl :: String -> String -> Either String Unit = "case call 'file':'write_file'($0, $1, ['append']) of <'ok'> when 'true' -> {'Right', 'unit'} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'atom_to_binary'(Reason, 'utf8')} end"

-- | Check if file exists
fileExists :: String -> Boolean
fileExists path = fileExistsImpl path

foreign import fileExistsImpl :: String -> Boolean = "call 'filelib':'is_regular'($0)"

-- | Check if directory exists
dirExists :: String -> Boolean
dirExists path = dirExistsImpl path

foreign import dirExistsImpl :: String -> Boolean = "call 'filelib':'is_dir'($0)"

-- | Create directory (including parents)
mkdirP :: String -> Either String Unit
mkdirP path = mkdirPImpl path

foreign import mkdirPImpl :: String -> Either String Unit = "case call 'filelib':'ensure_dir'(call 'erlang':'++'($0, \"/\")) of <'ok'> when 'true' -> {'Right', 'unit'} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'atom_to_binary'(Reason, 'utf8')} end"

-- | List directory contents
listDir :: String -> Either String (Array String)
listDir path = listDirImpl path

foreign import listDirImpl :: String -> Either String (Array String) = "case call 'file':'list_dir'($0) of <{'ok', Files}> when 'true' -> {'Right', call 'lists':'map'(fun (F) -> call 'erlang':'list_to_binary'(F), Files)} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'atom_to_binary'(Reason, 'utf8')} end"

-- | Delete a file
deleteFile :: String -> Either String Unit
deleteFile path = deleteFileImpl path

foreign import deleteFileImpl :: String -> Either String Unit = "case call 'file':'delete'($0) of <'ok'> when 'true' -> {'Right', 'unit'} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'atom_to_binary'(Reason, 'utf8')} end"

-- | Find all files with given extension recursively
-- | Extension should include the dot, e.g., ".purs"
findFilesRecursive :: String -> String -> Either String (Array String)
findFilesRecursive dir ext =
  case listDir dir of
    Left err -> Left err
    Right entries ->
      let
        processEntry entry =
          let fullPath = joinPath dir entry
          in if dirExists fullPath
             then case findFilesRecursive fullPath ext of
                    Left _ -> []
                    Right files -> files
             else if endsWith ext entry
                  then [fullPath]
                  else []
      in Right (concatMap processEntry entries)

-- | Get file basename
basename :: String -> String
basename path = basenameImpl path

foreign import basenameImpl :: String -> String = "call 'filename':'basename'($0)"

-- | Get file basename without extension
basenameNoExt :: String -> String -> String
basenameNoExt path ext = basenameNoExtImpl path ext

foreign import basenameNoExtImpl :: String -> String -> String = "call 'filename':'basename'($0, $1)"

-- | Get directory name
dirname :: String -> String
dirname path = dirnameImpl path

foreign import dirnameImpl :: String -> String = "call 'filename':'dirname'($0)"

-- | Join path components
joinPath :: String -> String -> String
joinPath a b = joinPathImpl a b

foreign import joinPathImpl :: String -> String -> String = "call 'filename':'join'($0, $1)"

-- | Get current working directory
cwd :: Either String String
cwd = cwdImpl

foreign import cwdImpl :: Either String String = "case call 'file':'get_cwd'() of <{'ok', Dir}> when 'true' -> {'Right', call 'erlang':'list_to_binary'(Dir)} <{'error', Reason}> when 'true' -> {'Left', call 'erlang':'atom_to_binary'(Reason, 'utf8')} end"

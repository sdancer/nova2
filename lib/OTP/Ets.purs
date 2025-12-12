-- | ETS - Erlang Term Storage bindings for Nova
module OTP.Ets where

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

-- | Table reference (Erlang reference or atom)
-- Note: Using Unit as a placeholder for opaque Erlang reference
type Table = Unit

-- | Table type
data TableType = Set | OrderedSet | Bag | DuplicateBag

-- | Access mode
data Access = Public | Protected | Private

-- | Table options
type TableOptions =
  { tableType :: TableType
  , access :: Access
  , named :: Boolean
  , keyPos :: Int
  }

-- | Default options: set, protected, not named, key at position 1
defaultOptions :: TableOptions
defaultOptions =
  { tableType: Set
  , access: Protected
  , named: false
  , keyPos: 1
  }

-- | Options for a public set table (most common for services)
publicSetOptions :: TableOptions
publicSetOptions =
  { tableType: Set
  , access: Public
  , named: true
  , keyPos: 1
  }

-- | Options for a public ordered set table
publicOrderedSetOptions :: TableOptions
publicOrderedSetOptions =
  { tableType: OrderedSet
  , access: Public
  , named: true
  , keyPos: 1
  }

-- | Create a new ETS table
new :: String -> TableOptions -> Either String Table
new name opts = newImpl name (tableTypeToAtom opts.tableType) (accessToAtom opts.access) opts.named opts.keyPos

tableTypeToAtom :: TableType -> String
tableTypeToAtom Set = "set"
tableTypeToAtom OrderedSet = "ordered_set"
tableTypeToAtom Bag = "bag"
tableTypeToAtom DuplicateBag = "duplicate_bag"

accessToAtom :: Access -> String
accessToAtom Public = "public"
accessToAtom Protected = "protected"
accessToAtom Private = "private"

foreign import newImpl :: String -> String -> String -> Boolean -> Int -> Either String Table
  = "let <Name> = call 'erlang':'list_to_atom'($0) in let <Type> = call 'erlang':'list_to_atom'($1) in let <Access> = call 'erlang':'list_to_atom'($2) in let <Opts0> = [Type, Access, {'keypos', $4}] in let <Opts> = case $3 of <'true'> when 'true' -> ['named_table' | Opts0] <'false'> when 'true' -> Opts0 end in case catch call 'ets':'new'(Name, Opts) of <{'EXIT', _R}> when 'true' -> {'Left', [101,114,114,111,114]} <Tab> when 'true' -> {'Right', Tab} end"

-- | Insert a key-value pair into the table
insert :: forall k v. Table -> k -> v -> Unit
insert tab key val = insertImpl tab key val

foreign import insertImpl :: forall k v. Table -> k -> v -> Unit
  = "let <_> = call 'ets':'insert'($0, {$1, $2}) in 'unit'"

-- | Lookup a value by key
lookup :: forall k v. Table -> k -> Maybe v
lookup tab key = lookupImpl tab key

foreign import lookupImpl :: forall k v. Table -> k -> Maybe v
  = "case call 'ets':'lookup'($0, $1) of <[{_K, V}]> when 'true' -> {'Just', V} <[]> when 'true' -> 'Nothing' <[T|_]> when 'true' -> {'Just', call 'erlang':'element'(2, T)} end"

-- | Check if a key exists in the table
member :: forall k. Table -> k -> Boolean
member tab key = memberImpl tab key

foreign import memberImpl :: forall k. Table -> k -> Boolean
  = "call 'ets':'member'($0, $1)"

-- | Delete a key from the table
delete :: forall k. Table -> k -> Unit
delete tab key = deleteImpl tab key

foreign import deleteImpl :: forall k. Table -> k -> Unit
  = "let <_> = call 'ets':'delete'($0, $1) in 'unit'"

-- | Delete the entire table
deleteTable :: Table -> Unit
deleteTable tab = deleteTableImpl tab

foreign import deleteTableImpl :: Table -> Unit
  = "let <_> = call 'ets':'delete'($0) in 'unit'"

-- | Get all keys from the table
allKeys :: forall k. Table -> Array k
allKeys tab = allKeysImpl tab

foreign import allKeysImpl :: forall k. Table -> Array k
  = "let <F> = fun (T, Acc) -> [call 'erlang':'element'(1, T) | Acc] in {'array', call 'ets':'foldl'(F, [], $0)}"

-- | Get all values from the table
allValues :: forall v. Table -> Array v
allValues tab = allValuesImpl tab

foreign import allValuesImpl :: forall v. Table -> Array v
  = "let <F> = fun (T, Acc) -> [call 'erlang':'element'(2, T) | Acc] in {'array', call 'ets':'foldl'(F, [], $0)}"

-- | Update a value in place (same as insert, overwrites)
update :: forall k v. Table -> k -> v -> Unit
update tab key val = updateImpl tab key val

foreign import updateImpl :: forall k v. Table -> k -> v -> Unit
  = "let <_> = call 'ets':'insert'($0, {$1, $2}) in 'unit'"

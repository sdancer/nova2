module Effect.Console where

-- | Effect type representing side-effecting computations
-- | At runtime this is just a thunk (0-arity function)
type Effect a = Unit -> a

-- | Print a string to the console
log :: String -> Effect Unit
log msg = logImpl msg

foreign import logImpl :: String -> Effect Unit = "fun () -> do call 'io':'put_chars'([$0, 10]) 'unit'"

-- | Print any value to the console (using Erlang term formatting)
logShow :: forall a. a -> Effect Unit
logShow val = logShowImpl val

foreign import logShowImpl :: forall a. a -> Effect Unit = "fun () -> do call 'io':'format'(\"~p~n\", [$0]) 'unit'"

-- | Print an error message to the console
error :: String -> Effect Unit
error msg = errorImpl msg

foreign import errorImpl :: String -> Effect Unit = "fun () -> do call 'io':'put_chars'('standard_error', [$0, 10]) 'unit'"

-- | Run an Effect (execute the side effect)
unsafePerformEffect :: forall a. Effect a -> a
unsafePerformEffect eff = unsafePerformEffectImpl eff

foreign import unsafePerformEffectImpl :: forall a. Effect a -> a = "apply $0 ()"

-- | Pure value lifted into Effect
pure :: forall a. a -> Effect a
pure a = pureImpl a

foreign import pureImpl :: forall a. a -> Effect a = "fun () -> $0"

-- | Sequence two effects, discarding the first result
bind :: forall a b. Effect a -> (a -> Effect b) -> Effect b
bind eff f = bindImpl eff f

foreign import bindImpl :: forall a b. Effect a -> (a -> Effect b) -> Effect b = "fun () -> let <R> = apply $0 () in let <Eff2> = apply $1 (R) in apply Eff2 ()"

-- | Sequence effects, discarding the result of the first
applySecond :: forall a b. Effect a -> Effect b -> Effect b
applySecond ea eb = applySecondImpl ea eb

foreign import applySecondImpl :: forall a b. Effect a -> Effect b -> Effect b = "fun () -> do apply $0 () apply $1 ()"

-- | Run effect and discard result
void :: forall a. Effect a -> Effect Unit
void eff = voidImpl eff

foreign import voidImpl :: forall a. Effect a -> Effect Unit = "fun () -> do apply $0 () 'unit'"

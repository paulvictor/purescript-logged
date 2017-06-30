module Log where

  import Prelude
  import Data.Foreign.Class
  import Data.Maybe (Maybe(..))
  import Data.Generic.Rep (class Generic)
  import Control.Monad.Eff
  import Control.Monad.State.Class
  import Data.Array
  import Partial.Unsafe
  import Data.Either
  import Control.Monad.Except
  import Data.Foreign.Generic
  import Control.Monad.State.Trans
  import Control.Monad.Eff.Console
  import Data.Tuple
  import Control.Monad.Eff.Class
  import Data.Foreign
  import Data.Generic
  import Data.Generic.Rep.Show
  import Control.Monad.Eff.Exception.Unsafe
  import Partial

  foreign import data READLOG :: Effect
  foreign import data WRITELOG :: Effect
  {--foreign import logStr :: ∀eff. Eff (readlog :: READLOG|eff) (Maybe String)--}
  {--foreign import emptyLog :: ∀eff. Eff (wlog :: WRITELOG|eff) Unit--}
  {--foreign import writeLog :: ∀eff. String → Eff (wlog :: WRITELOG|eff) Unit--}

  data LogEntry = Executing | Result String

  derive instance genericLogEntry :: Generic LogEntry _

  instance encodeLogEntry :: Encode LogEntry where
    encode = genericEncode defaultOptions
  instance decodeLogEntry :: Decode LogEntry where
    decode = genericDecode defaultOptions

  instance showLogEntry :: Show LogEntry where show = genericShow

  data Log = Log (Array LogEntry) (Array LogEntry)

  replayLog :: Log → Array LogEntry
  replayLog (Log _ l) = l

  accumulatedLog :: Log → Array LogEntry
  accumulatedLog (Log l _) = l

  derive instance genericLog :: Generic Log _

  instance showLog :: Show Log where show = genericShow
  instance encodeLog :: Encode Log where
    encode = genericEncode defaultOptions

  instance decodeLog :: Decode Log where
    decode = genericDecode defaultOptions

  runLog :: ∀m a. (Monad m) ⇒ StateT Log m a → Log → m a
  runLog s init = fst <$> runStateT s init

  logged :: ∀ m a eff. Monad m ⇒ Encode a ⇒ Decode a ⇒ MonadEff ( console :: CONSOLE | eff) m ⇒ StateT Log m a → StateT Log m a
  logged s = do
    (Log l replays) ← get
    x ← case uncons replays of
      Nothing → do -- We are running without logs
        modify \(Log a _) → Log (Executing : a) replays
        res ← s
        put $ Log ((Result $ encodeJSON res) : l) replays
        {--l' ← get--}
        {--_ ← liftEff $ log (encodeJSON l')--}
        pure res
      Just {head: x, tail: xs} → case x of
                                  Executing → do -- We have not yet executed the StateT
                                    modify \(Log a _) → Log (Executing : a) xs
                                    res ← s
                                    put $ Log ((Result $ encodeJSON res) : l) []
                                    pure res
                                  (Result r) → do
                                    let result = unsafePartial $ fromRight $ runExcept $ decodeJSON r
                                    put $ Log ((Result r) : l) xs
                                    pure result

    pure x

  flow :: ∀ m eff . Monad m ⇒ MonadEff ( console :: CONSOLE | eff ) m ⇒ StateT Log m Foreign
  flow = logged $ logged $ do
    r ← logged $ pure 5
    _ ← logged $ liftEff $ (toForeign <$> logShow (Tuple "A" r))
    _ ← logged $ liftEff $ (toForeign <$> logShow (Tuple "B" r))
    logged $ liftEff $ (toForeign <$> logShow (Tuple "C" r))

  main = void $ runLog flow (Log [] []) >>= (logShow <<< typeOf)

  savedFlow = "{\"contents\":[[{\"contents\":\"{}\",\"tag\":\"Result\"},{\"contents\":\"5\",\"tag\":\"Result\"},{\"tag\":\"Executing\"},{\"tag\":\"Executing\"}],[]],\"tag\":\"Log\"}"

  parsedSavedLog = unsafePartial $ fromRight $ ((runExcept $ decodeJSON savedFlow) :: Either _ Log)

  recover = void $ runLog flow (Log [] (reverse $ accumulatedLog parsedSavedLog)) >>= (logShow <<< typeOf)

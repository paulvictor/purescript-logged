module Log where

  import Prelude
  import Data.Foreign.Class (class Decode, class Encode)
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

  foreign import data READLOG :: Effect
  foreign import data WRITELOG :: Effect
  {--foreign import logStr :: ∀eff. Eff (readlog :: READLOG|eff) (Maybe String)--}
  {--foreign import emptyLog :: ∀eff. Eff (wlog :: WRITELOG|eff) Unit--}
  {--foreign import writeLog :: ∀eff. String → Eff (wlog :: WRITELOG|eff) Unit--}

  data LogEntry = Executing | Result String

  derive instance genericLogEntry :: Generic LogEntry _

  instance showLogEntry :: Show LogEntry where show = genericShow

  newtype Log = Log (Array LogEntry)

  derive instance genericLog :: Generic Log _

  instance showLog :: Show Log where show = genericShow

  runLog :: ∀m a. (Monad m) ⇒ StateT Log m a → m a
  runLog s = fst <$> runStateT s (Log [])

  {--logged :: ∀m a. (Monad m) ⇒ Encode a ⇒ StateT Log m a → StateT Log m a--}
  logged :: ∀ m a eff. Monad m ⇒ Encode a ⇒ MonadEff ( console :: CONSOLE | eff) m ⇒ StateT Log m a → StateT Log m a
  logged s = do
    (Log l) ← get
    modify \(Log a) → Log $ Executing : a
    res ← s
    put $ Log $ (Result $ encodeJSON res) : l
    l' ← get
    _ ← liftEff $ logShow l'
    pure res

  flow :: ∀ m eff . Monad m ⇒ MonadEff ( console :: CONSOLE | eff ) m ⇒ StateT Log m Foreign
  flow = logged $ do
    r ← logged $ pure 5
    _ ← logged $ liftEff $ (toForeign <$> logShow (Tuple "A" r))
    _ ← logged $ liftEff $ (toForeign <$> logShow (Tuple "B" r))
    logged $ liftEff $ (toForeign <$> logShow (Tuple "C" r))

  main = void $ runLog flow >>= (logShow <<< typeOf)

-- | App-level Supabase + Google Charts FFI.
--
-- Everything here calls small JS helpers defined in @static/index.html@.
-- We go through 'Supabase.Miso.Core' where the library glue works, and
-- through our own @app*@ helpers where it does not:
--
--   * @appInsertReading@: the library 'Supabase.Miso.Database.insert' never
--     fires its success callback because supabase-js returns @data: null@
--     unless @.select()@ is chained. We also need the generated row id back
--     (for undo), so we insert-and-select in one call.
--   * @appSignOut@: @supabase.auth.signOut()@ resolves with @{ error }@ only
--     (no @data@), so the library glue never invokes either callback.
--   * @appSignInGoogle@: OAuth needs @window.location@ for the redirect URL.
--   * @appDrawCalendar@: renders the Google Charts calendar into the
--     @#calendar-chart@ element outside of miso's virtual DOM.
--
-- All callbacks here are built with 'asyncCallback1' rather than the
-- library's 'Supabase.Miso.Core.successCallback' (which uses a sync
-- callback): a sync JS->WASM re-entry enqueues the action but does not
-- resume the miso scheduler, so the app would sit on the response until
-- the next DOM event happened to pump the runtime.
module Interop
  ( getSession
  , signInPassword
  , signUpPassword
  , signInGoogle
  , signOutEverywhere
  , insertReading
  , selectWithFilters
  , deleteFrom
  , drawCalendar
  ) where

import Control.Monad (void)
import Miso (Effect, withSink)
import Miso.DSL (Function (..), fromJSValUnchecked, jsg, toJSVal, (#))
import Miso.FFI (asyncCallback1)
import Miso.JSON (FromJSON, Result (..), Value, fromJSON, object, (.=))
import Miso.String (MisoString, ms)
import Supabase.Miso.Core (runSupabase, runSupabaseDelete, runSupabaseSelect)
import Supabase.Miso.Database (DeleteOptions, FetchOptions, Filter)

-- | Async replacement for 'Supabase.Miso.Core.successCallback' (see the
-- module header for why sync callbacks stall the app).
successCallback
  :: FromJSON t
  => (action -> IO ())
  -> (MisoString -> action)
  -> (t -> action)
  -> IO Function
successCallback sink errorful successful =
  Function <$> asyncCallback1 (\result ->
    fromJSON <$> fromJSValUnchecked result >>= \case
      Error msg -> sink (errorful (ms msg))
      Success value -> sink (successful value))

-- | Async replacement for 'Supabase.Miso.Core.errorCallback'.
errorCallback
  :: (action -> IO ())
  -> (MisoString -> action)
  -> IO Function
errorCallback sink errorful =
  Function <$> asyncCallback1 (\result ->
    fromJSON <$> fromJSValUnchecked result >>= \case
      Error msg -> sink (errorful (ms msg))
      Success value -> sink (errorful value))

-- | Current session (if any). Succeeds with @{ session: null }@ when logged out.
getSession
  :: (Value -> action)
  -> (MisoString -> action)
  -> Effect parent props model action
getSession successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  runSupabase "auth" "getSession" ([] :: [Value]) successful_ errorful_

-- | Email + password sign-in. Success payload is @{ user, session }@.
signInPassword
  :: MisoString
  -> MisoString
  -> (Value -> action)
  -> (MisoString -> action)
  -> Effect parent props model action
signInPassword email password successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  let credentials = object ["email" .= email, "password" .= password]
  runSupabase "auth" "signInWithPassword" [credentials] successful_ errorful_

-- | Email + password sign-up. When email confirmation is required the
-- success payload has @session: null@.
signUpPassword
  :: MisoString
  -> MisoString
  -> (Value -> action)
  -> (MisoString -> action)
  -> Effect parent props model action
signUpPassword email password successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  let credentials = object ["email" .= email, "password" .= password]
  runSupabase "auth" "signUp" [credentials] successful_ errorful_

-- | Google OAuth sign-in: navigates away to the provider, so only an error
-- callback is meaningful here.
signInGoogle
  :: (MisoString -> action)
  -> Effect parent props model action
signInGoogle errorful = withSink $ \sink -> do
  errorful_ <- errorCallback sink errorful
  void $ jsg "globalThis" # "appSignInGoogle" $ [errorful_]

-- | Sign out of the current session.
signOutEverywhere
  :: action
  -> (MisoString -> action)
  -> Effect parent props model action
signOutEverywhere successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful (\(_ :: Value) -> successful)
  errorful_ <- errorCallback sink errorful
  void $ jsg "globalThis" # "appSignOut" $ (successful_, errorful_)

-- | Insert a reading row and return the inserted row (with its id).
insertReading
  :: Value
  -> (Value -> action)
  -> (MisoString -> action)
  -> Effect parent props model action
insertReading row successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  row_ <- toJSVal row
  void $ jsg "globalThis" # "appInsertReading" $ (row_, successful_, errorful_)

-- | Like 'Supabase.Miso.Database.selectWithFilters', with async callbacks.
selectWithFilters
  :: MisoString
  -> MisoString
  -> [Filter]
  -> FetchOptions
  -> (Value -> action)
  -> (MisoString -> action)
  -> Effect parent props model action
selectWithFilters table columns filters fetchOptions successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  filters_ <- toJSVal filters
  fetchOptions_ <- toJSVal fetchOptions
  runSupabaseSelect table columns [filters_, fetchOptions_] successful_ errorful_

-- | Like 'Supabase.Miso.Database.deleteFrom', with async callbacks.
deleteFrom
  :: MisoString
  -> [Filter]
  -> DeleteOptions
  -> (Value -> action)
  -> (MisoString -> action)
  -> Effect parent props model action
deleteFrom table filters deleteOptions successful errorful = withSink $ \sink -> do
  successful_ <- successCallback sink errorful successful
  errorful_ <- errorCallback sink errorful
  filters_ <- toJSVal filters
  deleteOptions_ <- toJSVal deleteOptions
  runSupabaseDelete table [filters_, deleteOptions_] successful_ errorful_

-- | Draw the readings calendar. Rows are
-- @[{ y, m, d, v, tooltip }]@; the JS side retries until the Google Charts
-- loader and the container element are both ready.
drawCalendar :: Value -> IO ()
drawCalendar rows = do
  rows_ <- toJSVal rows
  void $ jsg "globalThis" # "appDrawCalendar" $ [rows_]

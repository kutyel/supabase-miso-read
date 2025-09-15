
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultilineStrings          #-}
{-# LANGUAGE OverloadedStrings         #-}

import Data.Aeson
import Miso
import Miso.Html.Element as H
import Miso.Html.Event as E
import Supabase.Miso.Storage

-------------------------------------------------------------------------------
-- model
-------------------------------------------------------------------------------

type Model = ()

mkModel :: Model
mkModel = ()

-------------------------------------------------------------------------------
-- action
-------------------------------------------------------------------------------

data Action
  = ActionError MisoString
  | ActionAskBuckets
  | ActionHandleBuckets [Value]
  | ActionAskFiles
  | ActionHandleFiles [Value]

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect parent Model Action
updateModel = \case

  ActionError errorMessage ->
    io_ $ consoleError errorMessage

  ActionAskBuckets ->
    listBuckets ActionHandleBuckets ActionError

  ActionHandleBuckets v ->
    io_ $ consoleLog $ ms $ show v

  ActionAskFiles ->
    listAllFiles "avatars" "test" ActionHandleFiles ActionError

  ActionHandleFiles v ->
    io_ $ consoleLog $ ms $ show v

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel _ = div_ []
  [ p_ [] [ "TODO" ]
  , button_ [ onClick ActionAskBuckets ] [ "list buckets" ]
  , button_ [ onClick ActionAskFiles ] [ "list all files" ]
  ]

-------------------------------------------------------------------------------
--  main
-------------------------------------------------------------------------------

main :: IO ()
main = 
  run $ startApp (component mkModel updateModel viewModel)
#ifndef WASM
    { scripts =
       [ Module
          """

          import { createClient } from 'https://cdn.jsdelivr.net/npm/@supabase/supabase-js/+esm'
          const supabase_url = 'https://cmeicmtkrdbrelovyssz.supabase.co';
          const supabase_key = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImNtZWljbXRrcmRicmVsb3Z5c3N6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NTY0NTM2MTMsImV4cCI6MjA3MjAyOTYxM30._ga2HbuYt8JJTKYEQZc5ACAP2VT3KyjcbbV1Og0wEG0' ;
          const supabase = createClient(supabase_url, supabase_key);
          globalThis['supabase'] = supabase;
          console.log('Supabase Instance: ', supabase)

          // dmj: usage like: runSupabase('auth','signUp', args, successCallback, errorCallback);
          globalThis['runSupabase'] = function (namespace, fnName, args, successful, errorful) {
            const p = ({ data, error }) => {
                if (data) successful(data);
                if (error) errorful(error);
              };
            if (Array.isArray(args) && !args.length>0) {
              globalThis['supabase'][namespace][fnName](args).then(p);
            } else {
              globalThis['supabase'][namespace][fnName]().then(p);
            }
          }

          globalThis['runSupabaseFrom'] = function (namespace, fromArg, fnName, args, successful, errorful) {
            const p = ({ data, error }) => {
                if (data) successful(data);
                if (error) errorful(error);
              };
            if (Array.isArray(args) && args.length>0) {
              globalThis['supabase'][namespace].from(fromArg)[fnName](args).then(p);
            } else {
              globalThis['supabase'][namespace].from(fromArg)[fnName]().then(p);
            }
          }

          """
       ]
    }
#endif

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif




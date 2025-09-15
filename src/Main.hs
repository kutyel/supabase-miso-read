
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
  | ActionAsk 
  | ActionHandle Value

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect parent Model Action
updateModel = \case

  ActionError errorMessage ->
    io_ $ consoleError errorMessage

  ActionAsk ->
    listBuckets' ActionHandle ActionError

  ActionHandle v ->
    io_ $ consoleLog $ ms $ show v

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel _ = div_ []
  [ p_ [] [ "TODO" ]
  , button_ [ onClick ActionAsk ] [ "ask supabase" ]
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
          // console.log('Supabase Instance: ', supabase)

          // dmj: usage like: runSupabase('auth','signUp', args, successCallback, errorCallback);
          globalThis['runSupabase'] = function (namespace, fnName, args, successful, errorful) {
            globalThis['supabase'][namespace][fnName](this, args).then(({ data, error }) => {
              if (data) successful(data);
              if (error) errorful(error);
            });
          }
          globalThis['runSupabaseFrom'] = function (namespace, from, fnName, args, successful, errorful) {
            globalThis['supabase'][namespace]['from'](from)[fnName](this, args).then(({ data, error }) => {
              if (data) successful(data);
              if (error) errorful(error);
            });
          }
          """
       ]
    }
#endif

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif




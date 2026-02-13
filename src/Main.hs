{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Aeson (Value)
import Miso
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Supabase.Miso.Auth

-------------------------------------------------------------------------------
-- model
-------------------------------------------------------------------------------

data Model = Model
    { email :: MisoString
    , password :: MisoString
    , authState :: AuthState
    , errorMsg :: Maybe MisoString
    }

data AuthState
    = LoggedOut
    | LoggingIn
    | LoggedIn User

mkModel :: Model
mkModel =
    Model
        { email = ""
        , password = ""
        , authState = LoggedOut
        , errorMsg = Nothing
        }

-------------------------------------------------------------------------------
-- action
-------------------------------------------------------------------------------

data Action
    = SetEmail MisoString
    | SetPassword MisoString
    | Login
    | HandleLoginSuccess AuthResponse
    | HandleLoginError MisoString
    | Logout
    | HandleLogoutSuccess Value
    | HandleLogoutError MisoString
    | NoOp

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect parent Model Action
updateModel = \case
    SetEmail e -> modify $ \m -> m{email = e}
    SetPassword p -> modify $ \m -> m{password = p}
    Login -> do
        m <- get
        modify $ \m' -> m'{authState = LoggingIn, errorMsg = Nothing}
        let creds =
                SignInCredentials
                    { sicEmail = Email (email m)
                    , sicPassword = Password (password m)
                    }
        signInWithPassword creds HandleLoginSuccess HandleLoginError
    HandleLoginSuccess AuthResponse{..} ->
        let user = adUser arData
         in modify $ \m ->
                m
                    { authState = LoggedIn user
                    , password = ""
                    , errorMsg = Nothing
                    }
    HandleLoginError err ->
        modify $ \m -> m{authState = LoggedOut, errorMsg = Just err}
    Logout ->
        signOut defaultSignOutOptions HandleLogoutSuccess HandleLogoutError
    HandleLogoutSuccess _ ->
        modify $ \m ->
            m
                { authState = LoggedOut
                , errorMsg = Nothing
                }
    HandleLogoutError err ->
        modify $ \m -> m{errorMsg = Just err}
    NoOp -> pure ()

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel Model{..} = case authState of
    LoggedIn user -> viewLoggedIn user
    _ -> viewLoginForm email password authState errorMsg

viewLoginForm :: MisoString -> MisoString -> AuthState -> Maybe MisoString -> View Model Action
viewLoginForm email_ password_ state errMsg =
    let isLoggingIn = case state of LoggingIn -> True; _ -> False
     in div_
            [P.class_ "login-container"]
            [ div_
                [P.class_ "card"]
                [ h2_ [P.class_ "card-title"] ["Login"]
                , div_
                    [P.class_ "card-content"]
                    [ div_
                        [P.class_ "field"]
                        [ label_ [P.for_ "email", P.class_ "label"] ["Email"]
                        , input_
                            [ P.type_ "email"
                            , P.class_ "input"
                            , P.id_ "email"
                            , P.placeholder_ "Enter your email"
                            , P.value_ email_
                            , P.disabled_ isLoggingIn
                            , E.onInput SetEmail
                            ]
                        ]
                    , div_
                        [P.class_ "field"]
                        [ label_ [P.for_ "password", P.class_ "label"] ["Password"]
                        , input_
                            [ P.type_ "password"
                            , P.class_ "input"
                            , P.id_ "password"
                            , P.placeholder_ "Enter your password"
                            , P.value_ password_
                            , P.disabled_ isLoggingIn
                            , E.onInput SetPassword
                            ]
                        ]
                    , case errMsg of
                        Nothing -> H.text ""
                        Just msg -> p_ [P.class_ "error-message"] [H.text msg]
                    , H.button_
                        [ P.class_ "btn btn-primary"
                        , P.disabled_ isLoggingIn
                        , E.onClick Login
                        ]
                        [if isLoggingIn then "Logging in..." else "Login"]
                    ]
                ]
            ]

viewLoggedIn :: User -> View Model Action
viewLoggedIn User{..} =
    div_
        [P.class_ "login-container"]
        [ div_
            [P.class_ "card"]
            [ h2_ [P.class_ "card-title"] ["Welcome!"]
            , div_
                [P.class_ "card-content"]
                [ p_ [] [H.text ("Logged in as " <> userEmail)]
                , H.button_
                    [ P.class_ "btn"
                    , E.onClick Logout
                    ]
                    ["Logout"]
                ]
            ]
        ]

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

main :: IO ()
main =
    run $ startApp (component mkModel updateModel viewModel)
#ifndef WASM
    { scripts =
       [ Module
          """
          import { createClient } from 'https://cdn.jsdelivr.net/npm/@supabase/supabase-js/+esm'
          const supabase_url = 'https://ljknwlqyxougfijkyybq.supabase.co';
          const supabase_key = 'sb_publishable_Ga2P7LzIDbJjbq3kka6-Ag_eYd91LLk';
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

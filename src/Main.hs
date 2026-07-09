{-# LANGUAGE CPP #-}

-- | Read the Bible — a reading tracker built with Miso + Supabase.
--
-- Port of https://github.com/kutyel/read-the-bible-svelte (Svelte +
-- Firebase) to Haskell (Miso, WASM) + Supabase, drawing the yearly
-- calendar heatmap with Google Charts.
module Main (main) where

import Data.List (sortOn)
import Data.Time
  ( Day
  , defaultTimeLocale
  , formatTime
  , fromGregorian
  , getCurrentTime
  , parseTimeM
  , toGregorian
  , utctDay
  )

import Miso
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Miso.JSON
import Miso.String (MisoString, fromMisoStringEither, ms)

import Bible qualified
import Interop (deleteFrom, selectWithFilters)
import Interop qualified
import Supabase.Miso.Database
  ( DeleteOptions (..)
  , FetchOptions (..)
  , eq
  , gte
  , lte
  )

-------------------------------------------------------------------------------
-- model
-------------------------------------------------------------------------------

data Model = Model
  { authState :: AuthState
  , email :: MisoString
  , password :: MisoString
  , notice :: Maybe Notice
  , today :: Day
  , selectedYear :: Integer
  , selectedBook :: MisoString
  , selectedChapter :: Int
  , selectedDate :: MisoString -- ^ ISO date, e.g. "2026-07-07"
  , readings :: [Reading]
  , loadingReadings :: Bool
  , chartDrawn :: Bool -- ^ once true, keep the old chart visible while reloading
  , lastInserted :: Maybe Int -- ^ id of the reading added last, for undo
  , darkTheme :: Bool
  }
  deriving (Eq)

data AuthState
  = Booting -- ^ waiting for getSession on startup
  | LoggedOut
  | LoggingIn
  | LoggedIn UserInfo
  deriving (Eq)

data UserInfo = UserInfo
  { userId :: MisoString
  , userEmail :: MisoString
  }
  deriving (Eq)

data Notice = ErrorNotice MisoString | InfoNotice MisoString
  deriving (Eq)

data Reading = Reading
  { readingId :: Int
  , readingBook :: MisoString
  , readingChapter :: Int
  , readingDate :: MisoString
  }
  deriving (Eq)

firstYear :: Integer
firstYear = 2020

mkModel :: Model
mkModel =
  Model
    { authState = Booting
    , email = ""
    , password = ""
    , notice = Nothing
    , today = fromGregorian firstYear 1 1
    , selectedYear = firstYear
    , selectedBook = "Genesis"
    , selectedChapter = 1
    , selectedDate = ""
    , readings = []
    , loadingReadings = False
    , chartDrawn = False
    , lastInserted = Nothing
    , darkTheme = True
    }

-------------------------------------------------------------------------------
-- JSON payloads
-------------------------------------------------------------------------------

instance FromJSON UserInfo where
  parseJSON = withObject "user" $ \o ->
    UserInfo <$> o .: "id" <*> o .: "email"

-- | The only part of a session object we care about: its user.
newtype SessionUser = SessionUser UserInfo

instance FromJSON SessionUser where
  parseJSON = withObject "session" $ \o -> SessionUser <$> o .: "user"

-- | Payload of @auth.getSession@: @{ session: null | { user, ... } }@.
newtype SessionPayload = SessionPayload (Maybe UserInfo)

instance FromJSON SessionPayload where
  parseJSON = withObject "session payload" $ \o -> do
    mSession <- o .:? "session"
    case mSession of
      Nothing -> pure (SessionPayload Nothing)
      Just Null -> pure (SessionPayload Nothing)
      Just sessionValue -> case fromJSON sessionValue of
        Success (SessionUser user) -> pure (SessionPayload (Just user))
        Error err -> fail (fromMisoStringToString err)

-- | Payload of @auth.signInWithPassword@ / @auth.signUp@:
-- @{ user, session: null | {...} }@. A null session after sign-up means
-- the account still needs email confirmation.
data AuthPayload = AuthPayload
  { apUser :: UserInfo
  , apHasSession :: Bool
  }

instance FromJSON AuthPayload where
  parseJSON = withObject "auth payload" $ \o -> do
    user <- o .: "user"
    mSession <- o .:? "session"
    let hasSession = case mSession of
          Nothing -> False
          Just Null -> False
          Just _ -> True
    pure (AuthPayload user hasSession)

instance FromJSON Reading where
  parseJSON = withObject "reading" $ \o ->
    Reading
      <$> o .: "id"
      <*> o .: "book"
      <*> o .: "chapter"
      <*> o .: "date"

-------------------------------------------------------------------------------
-- action
-------------------------------------------------------------------------------

data Action
  = Init
  | SetToday Day
  | HandleSession Value
  | SessionError MisoString
  | SetEmail MisoString
  | SetPassword MisoString
  | SignIn
  | SignUp
  | SignInGoogle
  | HandleAuth Value
  | AuthError MisoString
  | SignOut
  | SignedOut
  | SetYear MisoString
  | SetBook MisoString
  | SetChapter MisoString
  | SetDate MisoString
  | HandleReadings Value
  | MarkRead
  | HandleInserted Value
  | Unread
  | HandleDeleted Value
  | DbError MisoString
  | ToggleTheme
  | ThemeSet Bool

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect parent props Model Action
updateModel = \case
  Init -> do
    io (ThemeSet <$> Interop.isDarkTheme)
    io (SetToday . utctDay <$> getCurrentTime)
  SetToday day -> do
    let (year, _, _) = toGregorian day
    modify $ \m ->
      m
        { today = day
        , selectedYear = year
        , selectedDate = ms (formatTime defaultTimeLocale "%Y-%m-%d" day)
        }
    Interop.getSession HandleSession SessionError
  HandleSession value -> case fromJSON value of
    Success (SessionPayload (Just user)) -> loginAs user
    Success (SessionPayload Nothing) ->
      modify $ \m -> m {authState = LoggedOut}
    Error err ->
      modify $ \m ->
        m {authState = LoggedOut, notice = Just (ErrorNotice (ms err))}
  SessionError err ->
    modify $ \m ->
      m {authState = LoggedOut, notice = Just (ErrorNotice err)}
  SetEmail e -> modify $ \m -> m {email = e}
  SetPassword p -> modify $ \m -> m {password = p}
  SignIn -> do
    m <- get
    modify $ \m' -> m' {authState = LoggingIn, notice = Nothing}
    Interop.signInPassword (email m) (password m) HandleAuth AuthError
  SignUp -> do
    m <- get
    modify $ \m' -> m' {authState = LoggingIn, notice = Nothing}
    Interop.signUpPassword (email m) (password m) HandleAuth AuthError
  SignInGoogle -> do
    modify $ \m -> m {notice = Nothing}
    Interop.signInGoogle AuthError
  HandleAuth value -> case fromJSON value of
    Success AuthPayload {..}
      | apHasSession -> loginAs apUser
      | otherwise ->
          modify $ \m ->
            m
              { authState = LoggedOut
              , notice =
                  Just (InfoNotice "Check your inbox to confirm your account, then sign in.")
              }
    Error err ->
      modify $ \m ->
        m {authState = LoggedOut, notice = Just (ErrorNotice (ms err))}
  AuthError err ->
    modify $ \m ->
      m {authState = LoggedOut, notice = Just (ErrorNotice err)}
  SignOut -> Interop.signOutEverywhere SignedOut DbError
  SignedOut ->
    modify $ \m ->
      m
        { authState = LoggedOut
        , notice = Nothing
        , readings = []
        , chartDrawn = False
        , lastInserted = Nothing
        , password = ""
        }
  SetYear str -> case readInt str of
    Nothing -> pure ()
    Just year -> do
      modify $ \m -> m {selectedYear = toInteger year, loadingReadings = True}
      fetchReadings
  SetBook book ->
    modify $ \m -> m {selectedBook = book, selectedChapter = 1}
  SetChapter str -> case readInt str of
    Nothing -> pure ()
    Just chapter -> modify $ \m -> m {selectedChapter = chapter}
  SetDate date -> modify $ \m -> m {selectedDate = date}
  HandleReadings value -> case fromJSON value of
    Success (rows :: [Reading]) -> do
      let sorted = sortOn (\r -> (readingDate r, readingId r)) rows
      modify $ \m ->
        let m' = m {readings = sorted, loadingReadings = False}
         in case reverse sorted of
              lastRead : _ ->
                m'
                  { selectedBook = readingBook lastRead
                  , selectedChapter = readingChapter lastRead
                  }
              [] -> m'
      redrawCalendar
    Error err -> do
      modify $ \m ->
        m {loadingReadings = False, notice = Just (ErrorNotice (ms err))}
      redrawCalendar
  MarkRead -> do
    m <- get
    modify $ \m' -> m' {notice = Nothing}
    redrawCalendar
    Interop.insertReading
      ( object
          [ "book" .= selectedBook m
          , "chapter" .= selectedChapter m
          , "date" .= selectedDate m
          ]
      )
      HandleInserted
      DbError
  HandleInserted value -> do
    case fromJSON value of
      Success (row :: Reading) ->
        modify $ \m -> m {lastInserted = Just (readingId row)}
      Error _ -> pure ()
    fetchReadings
  Unread -> do
    m <- get
    case lastInserted m of
      Nothing -> pure ()
      Just rid ->
        deleteFrom
          "readings"
          [eq "id" rid]
          (DeleteOptions Nothing)
          HandleDeleted
          DbError
  HandleDeleted _ -> do
    modify $ \m -> m {lastInserted = Nothing}
    fetchReadings
  DbError err ->
    modify $ \m -> m {notice = Just (ErrorNotice err)}
  ToggleTheme ->
    io (ThemeSet <$> Interop.toggleTheme)
  ThemeSet dark -> do
    modify $ \m -> m {darkTheme = dark}
    m <- get
    -- the chart hardcodes theme colors, so redraw it in the new palette
    if chartDrawn m then redrawCalendar else pure ()

-- | Enter the logged-in state and load the current year's readings.
loginAs :: UserInfo -> Effect parent props Model Action
loginAs user = do
  modify $ \m ->
    m {authState = LoggedIn user, password = "", notice = Nothing}
  fetchReadings

-- | Load all readings of the selected year (RLS scopes rows to the user).
fetchReadings :: Effect parent props Model Action
fetchReadings = do
  m <- get
  modify $ \m' -> m' {loadingReadings = True}
  let year = ms (show (selectedYear m))
  selectWithFilters
    "readings"
    "*"
    [gte "date" (year <> "-01-01"), lte "date" (year <> "-12-31")]
    (FetchOptions Nothing Nothing)
    HandleReadings
    DbError

-- | Push the readings of the selected year into the Google Charts calendar.
redrawCalendar :: Effect parent props Model Action
redrawCalendar = do
  modify $ \m -> m {chartDrawn = True}
  m <- get
  io_ (Interop.drawCalendar (calendarRows m))

calendarRows :: Model -> Value
calendarRows m = toJSON (map row (readings m) `orIfEmpty` [placeholder])
  where
    orIfEmpty [] fallback = fallback
    orIfEmpty rows _ = rows
    -- an invisible zero-value marker keeps the selected year on screen
    -- when nothing has been read yet
    placeholder =
      object
        [ "y" .= selectedYear m
        , "m" .= (1 :: Int)
        , "d" .= (1 :: Int)
        , "v" .= (0 :: Int)
        ]
    row r =
      let (y, mo, d) = dateParts (readingDate r)
       in object
            [ "y" .= y
            , "m" .= mo
            , "d" .= d
            , "v" .= (1 :: Int)
            , "tooltip" .= tooltip r
            ]
    tooltip r =
      "<div style=\"font-size:1rem;padding:0.75rem;white-space:nowrap;\">"
        <> prettyDate (readingDate r)
        <> ": <strong>"
        <> readingBook r
        <> " "
        <> ms (show (readingChapter r))
        <> "</strong></div>"

-- | "2026-07-07" -> (2026, 7, 7); falls back to Jan 1 of the parsed year.
dateParts :: MisoString -> (Integer, Int, Int)
dateParts str =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (takeWhile (/= 'T') (show' str)) of
    Just day -> toGregorian (day :: Day)
    Nothing -> (firstYear, 1, 1)
  where
    show' = fromMisoStringToString

-- | "2026-07-07" -> "July 7, 2026" (like the original app's tooltips).
prettyDate :: MisoString -> MisoString
prettyDate str =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (takeWhile (/= 'T') (fromMisoStringToString str)) of
    Just (day :: Day) -> ms (formatTime defaultTimeLocale "%B %-d, %Y" day)
    Nothing -> str

fromMisoStringToString :: MisoString -> String
fromMisoStringToString = either (const "") id . fromMisoStringEither

readInt :: MisoString -> Maybe Int
readInt = either (const Nothing) Just . fromMisoStringEither

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel m@Model {..} =
  div_
    []
    [ themeToggle darkTheme
    , case authState of
        LoggedIn user -> viewApp user m
        Booting -> div_ [P.class_ "centered"] [spinner]
        _ -> viewLogin m
    ]

-- | Sun/moon button pinned to the top-right corner.
themeToggle :: Bool -> View Model Action
themeToggle dark =
  H.button_
    [ P.class_ "theme-toggle btn-icon-ghost"
    , P.type_ "button"
    , P.title_ (if dark then "Switch to light mode" else "Switch to dark mode")
    , E.onClick ToggleTheme
    ]
    [if dark then "🌞" else "🌙"]

viewLogin :: Model -> View Model Action
viewLogin Model {..} =
  let busy = authState == LoggingIn
   in div_
        [P.class_ "centered"]
        [ div_
            [P.class_ "card w-full max-w-sm"]
            [ header_
                []
                [ h2_ [] ["Read the Bible 📖"]
                , p_ [] ["Track your daily Bible reading"]
                ]
            , section_
                [P.class_ "form grid gap-6"]
                [ H.button_
                    [P.class_ "btn-outline w-full", P.type_ "button", disabledWhen busy, E.onClick SignInGoogle]
                    ["Sign in with Google"]
                , divider "or"
                , div_
                    [P.class_ "grid gap-2"]
                    [ H.label_ [P.for_ "email"] ["Email"]
                    , input_
                        [ P.type_ "email"
                        , P.id_ "email"
                        , P.placeholder_ "you@example.com"
                        , P.value_ email
                        , disabledWhen busy
                        , E.onInput SetEmail
                        ]
                    ]
                , div_
                    [P.class_ "grid gap-2"]
                    [ H.label_ [P.for_ "password"] ["Password"]
                    , input_
                        [ P.type_ "password"
                        , P.id_ "password"
                        , P.placeholder_ "••••••••"
                        , P.value_ password
                        , disabledWhen busy
                        , E.onInput SetPassword
                        ]
                    ]
                , viewNotice notice
                ]
            , footer_
                [P.class_ "flex gap-2"]
                [ H.button_
                    [P.class_ "btn flex-1", P.type_ "button", disabledWhen busy, E.onClick SignIn]
                    [if busy then "Signing in…" else "Sign in"]
                , H.button_
                    [P.class_ "btn-outline flex-1", P.type_ "button", disabledWhen busy, E.onClick SignUp]
                    ["Sign up"]
                ]
            ]
        ]

divider :: MisoString -> View Model Action
divider txt =
  div_
    [P.class_ "flex items-center gap-3 text-xs text-muted-foreground"]
    [ div_ [P.class_ "flex-1 border-t border-border"] []
    , text txt
    , div_ [P.class_ "flex-1 border-t border-border"] []
    ]

viewApp :: UserInfo -> Model -> View Model Action
viewApp user Model {..} =
  div_
    [P.class_ "app mx-auto flex flex-col gap-8 p-6"]
    [ div_
        [P.class_ "flex items-center justify-center gap-3"]
        [ span_ [P.class_ "badge-secondary"] [text (userEmail user)]
        , H.button_ [P.class_ "btn-sm-ghost", P.type_ "button", E.onClick SignOut] ["Sign out"]
        ]
    , div_
        [P.class_ "controls"]
        [ selectField "Year" (map (ms . show) [firstYear .. currentYear]) (ms (show selectedYear)) SetYear
        , selectField "Book" (map fst Bible.books) selectedBook SetBook
        , selectField "Chapter" (map (ms . show) [1 .. Bible.chaptersOf selectedBook]) (ms (show selectedChapter)) SetChapter
        , div_
            [P.class_ "field grid gap-2"]
            [ H.label_ [] ["Date"]
            , input_
                [ P.type_ "date"
                , P.value_ selectedDate
                , P.max_ (ms (formatTime defaultTimeLocale "%Y-%m-%d" today))
                , E.onChange SetDate
                ]
            ]
        , case lastInserted of
            Just _ ->
              H.button_
                [P.class_ "btn-destructive", P.type_ "button", E.onClick Unread]
                ["Undo ↩"]
            Nothing ->
              H.button_
                [P.class_ "btn", P.type_ "button", E.onClick MarkRead]
                ["Read"]
        ]
    , viewNotice notice
    , div_
        [P.class_ "card"]
        [ header_ [] [h2_ [] ["Have you read the Bible today?"]]
        , section_
            [P.class_ "chart-wrap flex overflow-x-auto"]
            [ div_ [P.id_ "calendar-chart"] []
            , if loadingReadings && not chartDrawn then spinner else text ""
            ]
        ]
    ]
  where
    (currentYear, _, _) = toGregorian today

selectField
  :: MisoString
  -> [MisoString]
  -> MisoString
  -> (MisoString -> Action)
  -> View Model Action
selectField labelText opts current toAction =
  div_
    [P.class_ "field grid gap-2"]
    [ H.label_ [] [text labelText]
    , select_
        [E.onChange toAction]
        [ option_ [P.value_ o, P.selected_ (o == current)] [text o]
        | o <- opts
        ]
    ]

viewNotice :: Maybe Notice -> View Model Action
viewNotice = \case
  Nothing -> text ""
  Just (ErrorNotice msg) -> p_ [P.class_ "text-sm text-center text-destructive"] [text msg]
  Just (InfoNotice msg) -> p_ [P.class_ "text-sm text-center text-muted-foreground"] [text msg]

spinner :: View Model Action
spinner = div_ [P.class_ "spinner animate-spin size-8 rounded-full mx-auto mt-4"] []

disabledWhen :: Bool -> Attribute Action
disabledWhen = boolProp "disabled"

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

app :: App Model Action
app = (component mkModel updateModel (const viewModel)) {mount = Just Init}

main :: IO ()
main = startApp defaultEvents app

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

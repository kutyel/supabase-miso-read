{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Read the Bible — a reading tracker built with Miso + Supabase.
--
-- Port of https://github.com/kutyel/read-the-bible-svelte (Svelte +
-- Firebase) to Haskell (Miso, WASM) + Supabase, drawing the yearly
-- calendar heatmap with Google Charts.
module Main (main) where

import Bible qualified
import Data.List (sortOn)
import Data.Maybe (isJust)
import Data.Time qualified as Time
import Interop (deleteFrom, selectWithFilters)
import Interop qualified
import Miso hiding (Object)
import GHC.Generics (Generic)
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Miso.JSON
import Miso.String (fromMisoStringEither)
import Supabase.Miso.Database qualified as Supabase

-------------------------------------------------------------------------------
-- model
-------------------------------------------------------------------------------

data Model = Model
  { authState :: AuthState,
    email :: MisoString,
    password :: MisoString,
    notice :: Maybe Notice,
    today :: Time.Day,
    selectedYear :: Integer,
    selectedBook :: MisoString,
    selectedChapter :: Int,
    -- | ISO date, e.g. "2026-07-07"
    selectedDate :: MisoString,
    readings :: [Reading],
    loadingReadings :: Bool,
    -- | once true, keep the old chart visible while reloading
    chartDrawn :: Bool,
    -- | id of the reading added last, for undo
    lastInserted :: Maybe Int,
    darkTheme :: Bool
  }
  deriving (Eq)

data AuthState
  = -- | waiting for getSession on startup
    Booting
  | LoggedOut
  | LoggingIn
  | LoggedIn UserInfo
  deriving (Eq)

data UserInfo = UserInfo
  { id :: MisoString,
    email :: MisoString
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON)

data Notice = ErrorNotice MisoString | InfoNotice MisoString
  deriving (Eq)

data Reading = Reading
  { id :: Int,
    book :: MisoString,
    chapter :: Int,
    date :: MisoString
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON)

newtype SessionUser = SessionUser {user :: UserInfo}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype SessionPayload = SessionPayload {session :: Maybe SessionUser}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data AuthPayload = AuthPayload
  { user :: UserInfo,
    session :: Maybe Object -- Nothing covers both absent and null
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

firstYear :: Integer
firstYear = 2020

mkModel :: Model
mkModel =
  Model
    { authState = Booting,
      email = "",
      password = "",
      notice = Nothing,
      today = Time.fromGregorian firstYear 1 1,
      selectedYear = firstYear,
      selectedBook = "Genesis",
      selectedChapter = 1,
      selectedDate = "",
      readings = [],
      loadingReadings = False,
      chartDrawn = False,
      lastInserted = Nothing,
      darkTheme = True
    }

-------------------------------------------------------------------------------
-- JSON payloads
-------------------------------------------------------------------------------

apHasSession :: AuthPayload -> Bool
apHasSession payload = isJust payload.session

-------------------------------------------------------------------------------
-- action
-------------------------------------------------------------------------------

data Action
  = Init
  | SetToday Time.Day
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
    io (SetToday . Time.utctDay <$> Time.getCurrentTime)
  SetToday day -> do
    let (year, _, _) = Time.toGregorian day
    modify $ \m ->
      m
        { today = day,
          selectedYear = year,
          selectedDate = ms (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d" day)
        }
    Interop.getSession HandleSession SessionError
  HandleSession value -> case fromJSON value of
    Success (SessionPayload (Just sessionUser)) -> loginAs sessionUser.user
    Success (SessionPayload Nothing) ->
      modify $ \m -> m {authState = LoggedOut}
    Error err ->
      modify $ \m ->
        m {authState = LoggedOut, notice = Just (ErrorNotice (ms err))}
  SessionError err ->
    modify $ \m ->
      m {authState = LoggedOut, notice = Just (ErrorNotice err)}
  SetEmail e -> modify $ \Model {..} -> Model {email = e, ..}
  SetPassword p -> modify $ \m -> m {password = p}
  SignIn -> do
    m <- get
    modify $ \m' -> m' {authState = LoggingIn, notice = Nothing}
    Interop.signInPassword (m.email) (m.password) HandleAuth AuthError
  SignUp -> do
    m <- get
    modify $ \m' -> m' {authState = LoggingIn, notice = Nothing}
    Interop.signUpPassword (m.email) (m.password) HandleAuth AuthError
  SignInGoogle -> do
    modify $ \m -> m {notice = Nothing}
    Interop.signInGoogle AuthError
  HandleAuth value -> case fromJSON value of
    Success (payload :: AuthPayload)
      | apHasSession payload -> loginAs payload.user
      | otherwise ->
          modify $ \m ->
            m
              { authState = LoggedOut,
                notice =
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
        { authState = LoggedOut,
          notice = Nothing,
          readings = [],
          chartDrawn = False,
          lastInserted = Nothing,
          password = ""
        }
  SetYear str -> case readInt str of
    Nothing -> pure ()
    Just year -> do
      modify $ \m -> m {selectedYear = toInteger year, loadingReadings = True}
      fetchReadings
  SetBook book ->
    modify $ \m ->
      m
        { selectedBook = book,
          selectedChapter =
            if m.selectedChapter <= Bible.chaptersOf book
              then m.selectedChapter
              else 1
        }
  SetChapter str -> case readInt str of
    Nothing -> pure ()
    Just chapter -> modify $ \m -> m {selectedChapter = chapter}
  SetDate date -> modify $ \m -> m {selectedDate = date}
  HandleReadings value -> case fromJSON value of
    Success (rows :: [Reading]) -> do
      let sorted = sortOn (\r -> (r.date, r.id)) rows
      modify $ \m ->
        let m' = m {readings = sorted, loadingReadings = False}
         in case reverse sorted of
              lastRead : _ ->
                m'
                  { selectedBook = lastRead.book,
                    selectedChapter = lastRead.chapter
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
          [ "book" .= m.selectedBook,
            "chapter" .= m.selectedChapter,
            "date" .= m.selectedDate
          ]
      )
      HandleInserted
      DbError
  HandleInserted value -> do
    case fromJSON value of
      Success (row :: Reading) ->
        modify $ \m -> m {lastInserted = Just row.id}
      Error _ -> pure ()
    fetchReadings
  Unread -> do
    m <- get
    case m.lastInserted of
      Nothing -> pure ()
      Just rid ->
        deleteFrom
          "readings"
          [Supabase.eq "id" rid]
          (Supabase.DeleteOptions Nothing)
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
    if m.chartDrawn then redrawCalendar else pure ()

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
  let year = ms (show (m.selectedYear))
  selectWithFilters
    "readings"
    "*"
    [Supabase.gte "date" (year <> "-01-01"), Supabase.lte "date" (year <> "-12-31")]
    (Supabase.FetchOptions Nothing Nothing)
    HandleReadings
    DbError

-- | Push the readings of the selected year into the Google Charts calendar.
redrawCalendar :: Effect parent props Model Action
redrawCalendar = do
  modify $ \m -> m {chartDrawn = True}
  m <- get
  io_ (Interop.drawCalendar (calendarRows m))

calendarRows :: Model -> Value
calendarRows m = toJSON (map row (m.readings) `orIfEmpty` [placeholder])
  where
    orIfEmpty [] fallback = fallback
    orIfEmpty rows _ = rows
    -- an invisible zero-value marker keeps the selected year on screen
    -- when nothing has been read yet
    placeholder =
      object
        [ "y" .= m.selectedYear,
          "m" .= (1 :: Int),
          "d" .= (1 :: Int),
          "v" .= (0 :: Int)
        ]
    row r =
      let (y, mo, d) = dateParts r.date
       in object
            [ "y" .= y,
              "m" .= mo,
              "d" .= d,
              "v" .= (1 :: Int),
              "tooltip" .= tooltip r
            ]
    tooltip r =
      "<div style=\"font-size:1rem;padding:0.75rem;white-space:nowrap;\">"
        <> prettyDate r.date
        <> ": <strong>"
        <> r.book
        <> " "
        <> ms (show r.chapter)
        <> "</strong></div>"

-- | "2026-07-07" -> (2026, 7, 7); falls back to Jan 1 of the parsed year.
dateParts :: MisoString -> (Integer, Int, Int)
dateParts str =
  case Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" (takeWhile (/= 'T') (show' str)) of
    Just day -> Time.toGregorian (day :: Time.Day)
    Nothing -> (firstYear, 1, 1)
  where
    show' = fromMisoStringToString

-- | "2026-07-07" -> "July 7, 2026" (like the original app's tooltips).
prettyDate :: MisoString -> MisoString
prettyDate str =
  case Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" (takeWhile (/= 'T') (fromMisoStringToString str)) of
    Just (day :: Time.Day) -> ms (Time.formatTime Time.defaultTimeLocale "%B %-d, %Y" day)
    Nothing -> str

fromMisoStringToString :: MisoString -> String
fromMisoStringToString = either (const "") id . fromMisoStringEither

readInt :: MisoString -> Maybe Int
readInt = either (const Nothing) Just . fromMisoStringEither

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View () () Model Action
viewModel m@Model {..} =
  div_
    []
    [ themeToggle darkTheme,
      case authState of
        LoggedIn user -> viewApp user m
        Booting -> div_ [P.class_ "centered"] [spinner]
        _ -> viewLogin m
    ]

-- | Sun/moon button pinned to the top-right corner.
themeToggle :: Bool -> View () () Model Action
themeToggle dark =
  H.button_
    [ P.class_ "theme-toggle btn-icon-ghost",
      P.type_ "button",
      P.title_ (if dark then "Switch to light mode" else "Switch to dark mode"),
      E.onClick ToggleTheme
    ]
    [if dark then "🌞" else "🌙"]

viewLogin :: Model -> View () () Model Action
viewLogin Model {..} =
  let busy = authState == LoggingIn
   in div_
        [P.class_ "centered"]
        [ div_
            [P.class_ "card w-full max-w-sm"]
            [ header_
                []
                [ h2_ [] ["Read the Bible 📖"],
                  p_ [] ["Track your daily Bible reading"]
                ],
              section_
                [P.class_ "form grid gap-6"]
                [ H.button_
                    [P.class_ "btn-outline w-full", P.type_ "button", disabledWhen busy, E.onClick SignInGoogle]
                    ["Sign in with Google"],
                  divider "or",
                  div_
                    [P.class_ "grid gap-2"]
                    [ H.label_ [P.for_ "email"] ["Email"],
                      input_
                        [ P.type_ "email",
                          P.id_ "email",
                          P.placeholder_ "you@example.com",
                          P.value_ email,
                          disabledWhen busy,
                          E.onInput SetEmail
                        ]
                    ],
                  div_
                    [P.class_ "grid gap-2"]
                    [ H.label_ [P.for_ "password"] ["Password"],
                      input_
                        [ P.type_ "password",
                          P.id_ "password",
                          P.placeholder_ "••••••••",
                          P.value_ password,
                          disabledWhen busy,
                          E.onInput SetPassword
                        ]
                    ],
                  viewNotice notice
                ],
              footer_
                [P.class_ "flex gap-2"]
                [ H.button_
                    [P.class_ "btn flex-1", P.type_ "button", disabledWhen busy, E.onClick SignIn]
                    [if busy then "Signing in…" else "Sign in"],
                  H.button_
                    [P.class_ "btn-outline flex-1", P.type_ "button", disabledWhen busy, E.onClick SignUp]
                    ["Sign up"]
                ]
            ]
        ]

divider :: MisoString -> View () () Model Action
divider txt =
  div_
    [P.class_ "flex items-center gap-3 text-xs text-muted-foreground"]
    [ div_ [P.class_ "flex-1 border-t border-border"] [],
      text txt,
      div_ [P.class_ "flex-1 border-t border-border"] []
    ]

viewApp :: UserInfo -> Model -> View () () Model Action
viewApp user Model {..} =
  div_
    [P.class_ "app mx-auto flex flex-col gap-8 p-6"]
    [ div_
        [P.class_ "flex items-center justify-center gap-3"]
        [ span_ [P.class_ "badge-secondary"] [text user.email],
          H.button_ [P.class_ "btn-sm-ghost", P.type_ "button", E.onClick SignOut] ["Sign out"]
        ],
      div_
        [P.class_ "controls"]
        [ selectField "Year" (map (ms . show) [firstYear .. currentYear]) (ms (show selectedYear)) SetYear,
          selectField "Book" (map fst Bible.books) selectedBook SetBook,
          selectField "Chapter" (map (ms . show) [1 .. Bible.chaptersOf selectedBook]) (ms (show selectedChapter)) SetChapter,
          div_
            [P.class_ "field grid gap-2"]
            [ H.label_ [] ["Date"],
              input_
                [ P.type_ "date",
                  P.value_ selectedDate,
                  P.max_ (ms (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d" today)),
                  E.onChange SetDate
                ]
            ],
          case lastInserted of
            Just _ ->
              H.button_
                [P.class_ "btn-destructive", P.type_ "button", E.onClick Unread]
                ["Undo ↩"]
            Nothing ->
              H.button_
                [P.class_ "btn", P.type_ "button", E.onClick MarkRead]
                ["Read"]
        ],
      viewNotice notice,
      div_
        [P.class_ "card"]
        [ header_ [] [h2_ [] ["Have you read the Bible today?"]],
          section_
            [P.class_ "chart-wrap flex overflow-x-auto"]
            [ div_ [P.id_ "calendar-chart"] [],
              if loadingReadings && not chartDrawn then spinner else text ""
            ]
        ]
    ]
  where
    (currentYear, _, _) = Time.toGregorian today

selectField ::
  MisoString ->
  [MisoString] ->
  MisoString ->
  (MisoString -> Action) ->
  View () () Model Action
selectField labelText opts current toAction =
  div_
    [P.class_ "field grid gap-2"]
    [ H.label_ [] [text labelText],
      select_
        [E.onChange toAction]
        [ option_ [P.value_ o, P.selected_ (o == current)] [text o]
          | o <- opts
        ]
    ]

viewNotice :: Maybe Notice -> View () () Model Action
viewNotice = \case
  Nothing -> text ""
  Just (ErrorNotice msg) -> p_ [P.class_ "text-sm text-center text-destructive"] [text msg]
  Just (InfoNotice msg) -> p_ [P.class_ "text-sm text-center text-muted-foreground"] [text msg]

spinner :: View () () Model Action
spinner = div_ [P.class_ "spinner animate-spin size-8 rounded-full mx-auto mt-4"] []

disabledWhen :: Bool -> Attribute Model Action
disabledWhen = boolProp "disabled"

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

app :: App Model Action
app = (component mkModel updateModel viewModel) {mount = Just Init}

main :: IO ()
main = startApp defaultEvents app

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

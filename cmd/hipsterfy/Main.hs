module Main (main) where

import Control.Concurrent (myThreadId, throwTo)
import Control.Monad.Except (throwError)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Faktory.Client (Client, newClient)
import Faktory.Settings (ConnectionInfo (..), Queue, Settings (..))
import qualified Faktory.Settings as Faktory (defaultSettings)
import Hipsterfy.Jobs.UpdateUser (enqueueUpdateUser, updateUserQueue)
import Hipsterfy.Pages (accountPage, comparePage, loginPage)
import Hipsterfy.Session (endSession, getSession, startSession)
import Hipsterfy.Spotify.Auth
  ( Scope,
    SpotifyApp (..),
    scopeUserFollowRead,
    scopeUserLibraryRead,
    scopeUserTopRead,
  )
import Hipsterfy.User
  ( User (..),
    createOAuthRedirect,
    createUser,
    getFollowedArtists,
    getUpdateStatus,
    getUserByFriendCode,
  )
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Options.Applicative
  ( ParserInfo,
    auto,
    briefDesc,
    execParser,
    helper,
    info,
    long,
    option,
    progDesc,
    strOption,
  )
import Relude
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import System.Posix (Handler (Catch))
import System.Posix.Signals (installHandler, softwareTermination)
import Web.Scotty (ActionM, ScottyM, html, middleware, param, redirect, scottyOpts)
import qualified Web.Scotty as S
import Web.Scotty.Internal.Types (ActionError (Redirect))

data Options = Options
  { host :: Text,
    port :: Int,
    clientID :: Text,
    clientSecret :: Text,
    pgConn :: Text,
    faktoryHost :: Text,
    faktoryPort :: Int,
    faktoryPassword :: Text
  }
  deriving (Show)

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (briefDesc <> progDesc "Hipsterfy server")
  where
    options =
      Options
        <$> strOption (long "host")
        <*> option auto (long "port")
        <*> strOption (long "client_id")
        <*> strOption (long "client_secret")
        <*> strOption (long "db")
        <*> strOption (long "faktory_host")
        <*> option auto (long "faktory_port")
        <*> strOption (long "faktory_password")

main :: IO ()
main = do
  tid <- myThreadId
  _ <- installHandler softwareTermination (Catch $ throwTo tid ExitSuccess) Nothing
  hSetBuffering stdout NoBuffering
  options <- execParser opts
  runServer options

runServer :: Options -> IO ()
runServer Options {host, port, pgConn, clientID, clientSecret, faktoryHost, faktoryPassword, faktoryPort} = do
  conn <- connectPostgreSQL $ encodeUtf8 pgConn
  updateUserClient <- newClient (settingsForQ updateUserQueue) Nothing

  putStrLn $ "Starting server at: " `mappend` show address
  scottyOpts
    S.Options {verbose = 0, settings = defaultSettings & setPort port}
    $ server spotifyApp updateUserClient conn
  where
    address :: Text
    address = host `mappend` case port of
      80 -> ""
      other -> ":" `mappend` show other
    spotifyApp :: SpotifyApp
    spotifyApp = SpotifyApp {clientID, clientSecret, redirectURI = address <> "/authorize/callback"}
    settingsForQ :: Queue -> Settings
    settingsForQ queue =
      Faktory.defaultSettings
        { settingsQueue = queue,
          settingsConnection =
            ConnectionInfo
              { connectionInfoTls = False,
                connectionInfoHostName = toString faktoryHost,
                connectionInfoPassword = Just $ toString faktoryPassword,
                connectionInfoPort = fromInteger $ toInteger faktoryPort
              }
        }

server :: SpotifyApp -> Client -> Connection -> ScottyM ()
server spotifyApp updateUserClient conn = do
  middleware logStdoutDev

  -- Home page. Check cookies to see if logged in.
  -- If not logged in, prompt to authorize.
  -- If logged in, provide friend code input.
  S.get "/" $ do
    user <- getSession conn
    case user of
      Just u@User {userID} -> do
        void $ enqueueUpdateUser updateUserClient conn userID
        followed <- getFollowedArtists conn userID
        status <- getUpdateStatus conn userID
        html $ accountPage u status followed
      Nothing -> html loginPage

  -- Authorization redirect. Generate a new user's OAuth secret and friend code. Redirect to Spotify.
  S.get "/authorize" $ createOAuthRedirect spotifyApp conn spotifyScopes >>= redirect

  -- Authorization callback. Populate a user's Spotify information based on the callback. Set cookies to logged in. Redirect to home page.
  S.get "/authorize/callback" $ do
    -- If a session is already set, then ignore this request.
    session <- getSession conn
    case session of
      Just _ -> throwError $ Redirect "/"
      Nothing -> pass

    -- Obtain access tokens.
    code <- param "code" :: ActionM Text
    oauthState <- param "state" :: ActionM Text
    eitherUser <- createUser spotifyApp conn code oauthState
    user <- case eitherUser of
      Right user -> return user
      Left err -> do
        print err
        throwError $ Redirect "/"

    -- Create a new session.
    startSession conn user

    -- Redirect to dashboard.
    redirect "/"

  -- Compare your artists against a friend code. Must be logged in.
  S.post "/compare" $ do
    -- Require authentication.
    maybeUser <- getSession conn
    user <- case maybeUser of
      Just u -> return u
      Nothing -> throwError $ Redirect "/"

    -- Check that the friend code is valid.
    friendCode <- param "friend-code" :: ActionM Text
    maybeFriend <- getUserByFriendCode conn friendCode
    friend <- case maybeFriend of
      Just f -> return f
      -- TODO: display an error message for invalid friend codes.
      Nothing -> throwError $ Redirect "/"

    -- Load followed artists.
    yourArtists <- getFollowedArtists conn $ userID user
    friendArtists <- getFollowedArtists conn $ userID friend

    -- Render page.
    html $ comparePage yourArtists friendArtists

  -- Clear logged in cookies.
  S.get "/logout" $ do
    endSession conn
    redirect "/"
  where
    spotifyScopes :: [Scope]
    spotifyScopes = [scopeUserLibraryRead, scopeUserFollowRead, scopeUserTopRead]

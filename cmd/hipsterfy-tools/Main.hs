module Main (main) where

import Database.PostgreSQL.Simple (connectPostgreSQL)
import Hipsterfy.Spotify (SpotifyUserID (SpotifyUserID))
import Hipsterfy.Spotify.Auth (SpotifyApp (..))
import Hipsterfy.User (User (..), getUserBySpotifyID, refreshCredentialsIfNeeded)
import Options.Applicative
  ( Parser,
    ParserInfo,
    argument,
    briefDesc,
    command,
    execParser,
    helper,
    hsubparser,
    info,
    long,
    metavar,
    progDesc,
    str,
    strOption,
  )
import Relude

{- HLINT ignore Options "Use newtype instead of data" -}
data Options = Options
  { cmd :: Command
  }

{- HLINT ignore Command "Use newtype instead of data" -}
data Command
  = GetToken GetTokenOptions

data GetTokenOptions = GetTokenOptions
  { clientID :: Text,
    clientSecret :: Text,
    pgConn :: Text,
    spotifyUserID :: Text
  }

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (briefDesc <> progDesc "Hipsterfy developer tools")
  where
    options =
      Options
        <$> hsubparser
          (command "get-access-token" (GetToken <$> getTokenCmd))
    getTokenCmd :: ParserInfo GetTokenOptions
    getTokenCmd =
      info
        (getTokenOptions <**> helper)
        (briefDesc <> progDesc "Retrieve an access token from a user")
    getTokenOptions :: Parser GetTokenOptions
    getTokenOptions =
      GetTokenOptions
        <$> strOption (long "client_id")
        <*> strOption (long "client_secret")
        <*> strOption (long "db")
        <*> argument str (metavar "spotify_user_id")

main :: IO ()
main = do
  Options {cmd} <- execParser opts
  case cmd of
    GetToken opts' -> do
      let GetTokenOptions {spotifyUserID, clientID, clientSecret, pgConn} = opts'
      let app = SpotifyApp {clientID, clientSecret, redirectURI = error "error: redirectURI never used"}
      conn <- connectPostgreSQL $ encodeUtf8 pgConn
      maybeUser <- getUserBySpotifyID conn $ SpotifyUserID spotifyUserID
      User {userID, spotifyCredentials} <- case maybeUser of
        Just u -> return u
        Nothing -> error $ "error: could not find user with Spotify ID " <> show spotifyUserID
      creds <- refreshCredentialsIfNeeded app conn userID spotifyCredentials
      print creds

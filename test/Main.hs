module Main (main) where

import Hipsterfy.Spotify.Auth (SpotifyCredentials (..))
import Hipsterfy.Spotify.Spec (testGetAlbums)
import Options.Applicative
  ( ParserInfo,
    briefDesc,
    execParser,
    helper,
    info,
    long,
    progDesc,
    strOption,
  )
import Relude
import System.Environment (withArgs)
import Test.Hspec (hspec)

newtype Options = Options
  { accessToken :: Text
  }

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (briefDesc <> progDesc "Hipsterfy automated test")
  where
    options =
      Options
        <$> strOption (long "access_token")

main :: IO ()
main = do
  Options {accessToken} <- execParser opts
  let creds =
        SpotifyCredentials
          { accessToken,
            refreshToken = error "impossible: refreshToken never used",
            expiration = error "impossible: expiration never used"
          }
  withArgs [] $ hspec $ do
    testGetAlbums creds

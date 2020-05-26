module Hipsterfy (runServer, Options (..)) where

import Control.Lens ((.~))
import Data.Text.Encoding.Base64 (encodeBase64)
import Database.PostgreSQL.Simple (Connection, Only (Only), connectPostgreSQL, execute, query_)
import Network.HTTP.Types (renderSimpleQuery)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wreq (FormParam ((:=)), defaults, header, postWith)
import Relude
import Test.RandomStrings (randomASCII, randomWord)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!), Html, docTypeHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ActionM, ScottyM, scottyOpts)
import qualified Web.Scotty as S

data Options = Options
  { host :: Text,
    port :: Int,
    pgConn :: ByteString,
    clientID :: Text,
    clientSecret :: Text
  }
  deriving (Show)

runServer :: Options -> IO ()
runServer (Options {host, port, pgConn, clientID, clientSecret}) = do
  conn <- connectPostgreSQL pgConn

  putStrLn $ "Starting server at: " `mappend` (show address)
  scottyOpts
    S.Options {verbose = 0, settings = defaultSettings & setPort port}
    $ app conn
  where
    -- TODO: how is there no standard library for joining URLs?
    spotifyAuthURL = "https://accounts.spotify.com/authorize"
    spotifyTokenURL = "https://accounts.spotify.com/api/token"
    address = host `mappend` case port of
      80 -> ""
      other -> ":" `mappend` show other

    app :: Connection -> ScottyM ()
    app conn = do
      -- Home page. Check cookies to see if logged in.
      -- If not logged in, prompt to authorize.
      -- If logged in, provide friend code input.
      S.get "/" $ do
        -- TODO: check cookies, provide logged in view.
        S.html $ renderHtml homepage

      -- Authorization redirect. Generate a new user's OAuth secret and friend code. Redirect to Spotify.
      S.get "/authorize" $ do
        friendCode <- liftIO $ randomWord randomASCII 20
        oauthSecret <- liftIO $ randomWord randomASCII 20
        _ <- liftIO $ execute conn "INSERT INTO hipsterfy_user (friend_code, oauth2_secret) VALUES (?, ?)" (friendCode, oauthSecret)
        let qs =
              decodeUtf8 $
                renderSimpleQuery
                  True
                  [ ("client_id", encodeUtf8 clientID),
                    ("response_type", "code"),
                    ("redirect_uri", (encodeUtf8 address) <> "/authorize/callback"),
                    ("state", encodeUtf8 oauthSecret),
                    ("scope", "user-library-read user-top-read user-follow-read")
                  ]
        S.redirect $ spotifyAuthURL <> qs

      -- Authorization callback. Populate a user's Spotify information based on the callback. Set cookies to logged in. Redirect to home page.
      S.get "/authorize/callback" $ do
        code <- S.param "code" :: ActionM Text
        oauthSecret <- S.param "state" :: ActionM Text
        res <-
          liftIO $
            postWith
              (defaults & header "Authorization" .~ ["Basic " <> encodeUtf8 (encodeBase64 $ clientID <> ":" <> clientSecret)])
              spotifyTokenURL
              [ "grant_type" := ("authorization_code" :: Text),
                "code" := code,
                "redirect_uri" := address <> "/authorize/callback"
              ]
        print res
        -- TODO: save access token and set cookies.
        S.redirect "/"

      -- Compare your artists against a friend code. Must be logged in.
      S.get "/compare" $ do
        undefined

      -- Clear logged in cookies.
      S.get "/logout" $ do
        undefined

      S.get "/debug" $ do
        [Only i] <- liftIO $ (query_ conn "SELECT COUNT(*) FROM hipsterfy_user" :: IO [Only Int])
        S.text $ show i

homepage :: Html
homepage = docTypeHtml $ do
  H.head
    $ H.title
    $ "Hipsterfy"
  H.body
    $ H.a ! A.href "/authorize"
    $ "Authorize with Spotify"

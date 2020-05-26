module Hipsterfy (runServer, ServerOptions (..)) where

import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Relude
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5
import Text.Blaze.Html5 as H
import Web.Scotty
import Web.Scotty as S
import Database.PostgreSQL.Simple

data ServerOptions = ServerOptions
  { port :: Int,
    pgConn :: ByteString
  }

runServer :: ServerOptions -> IO ()
runServer (ServerOptions {port, pgConn}) = do
  conn <- connectPostgreSQL pgConn

  scottyOpts
    Options {verbose = 0, settings = defaultSettings & setPort port}
    $ app conn
  where
    app :: Connection -> ScottyM ()
    app conn = do

      S.get "/" $ do
        S.html $ renderHtml homepage

      S.get "/debug" $ do
        [Only i] <- liftIO $ (query_ conn "select count(*) from hipsterfy_user" :: IO [Only Int])
        S.text $ show i

homepage :: Html
homepage = docTypeHtml $ do
  H.head
    $ H.title
    $ H.toHtml ("Hipsterfy" :: Text)
  H.body $ H.toHtml ("Hello, hipster!" :: Text)

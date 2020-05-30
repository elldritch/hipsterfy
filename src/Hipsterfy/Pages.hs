module Hipsterfy.Pages (loginPage, accountPage, comparePage) where

import Data.Text.Lazy (Text, pack)
import Hipsterfy.Spotify (SpotifyArtist, SpotifyArtistInsights)
import Hipsterfy.User (User (..))
import Relude hiding (Text)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!), Html, docTypeHtml, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

render :: Html -> Text
render body = renderHtml $ docTypeHtml $ do
  H.head $ H.title "Hipsterfy"
  H.body body

loginPage :: Text
loginPage = render $ do
  H.a ! A.href "/authorize" $ "Authorize with Spotify"

accountPage :: User -> Text
accountPage user = render $ do
  H.p $ "Logged in as " <> toHtml (spotifyUserID user)
  H.form ! A.action "/compare" ! A.method "POST" $ do
    H.label "Add friend code:"
    H.input ! A.type_ "text" ! A.name "friend-code"
    H.input ! A.type_ "submit"
  H.a ! A.href "/logout" $ "Log out"

comparePage :: [(SpotifyArtist, SpotifyArtistInsights)] -> Text
comparePage yourFollowedArtists = render $ do
  H.p "your artists: " <> toHtml (mconcat $ fmap (pack . show) yourFollowedArtists)

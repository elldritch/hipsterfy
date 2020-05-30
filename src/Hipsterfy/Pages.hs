module Hipsterfy.Pages (loginPage, accountPage, comparePage) where

import Data.List (intersect)
import Data.Text.Lazy (Text)
import Hipsterfy.Spotify (SpotifyArtist, SpotifyArtistInsights, monthlyListeners, name)
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

comparePage :: [(SpotifyArtist, SpotifyArtistInsights)] -> [(SpotifyArtist, SpotifyArtistInsights)] -> Text
comparePage yourFollowedArtists friendFollowedArtists = render $ do
  H.p "Your mutual artists: " <> toHtml (mconcat $ fmap (name . fst) both)
  where
    sortHipster :: [(SpotifyArtist, SpotifyArtistInsights)] -> [(SpotifyArtist, SpotifyArtistInsights)]
    sortHipster = sortBy (\(_, i) (_, i') -> comparing monthlyListeners i i')
    yours = sortHipster yourFollowedArtists
    friends = sortHipster friendFollowedArtists
    both = sortHipster $ intersect yours friends

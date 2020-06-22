module Hipsterfy.Pages (loginPage, accountPage, comparePage) where

import Data.List (intersect)
import Data.List.Split (chunksOf)
import Hipsterfy.Spotify (SpotifyArtist (..), SpotifyArtistInsights (..), monthlyListeners, name)
import Hipsterfy.User (UpdateStatus (..), User (..))
import Relude hiding (Text, div, head)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 hiding (body, contents)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

render :: Html -> LText
render body = renderHtml $ docTypeHtml $ do
  head $ do
    title "Hipsterfy"
    meta ! A.name "description" ! A.content "See which hipsters you and your friends both follow"
    link ! A.href "https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css" ! A.rel "stylesheet"
    link ! A.href "https://cdn.jsdelivr.net/npm/@tailwindcss/ui@latest/dist/tailwind-ui.min.css" ! A.rel "stylesheet"

  H.body body

container :: Html -> LText
container contents = render $ do
  div ! A.class_ "min-h-screen flex flex-col py-12" $ do
    div ! A.class_ "mx-auto" $ do
      h1 ! A.class_ "text-3xl mb-2" $ "Hipsterfy"
      contents

loginPage :: LText
loginPage = render $ do
  div ! A.class_ "min-h-screen flex flex-col py-12" $ do
    div ! A.class_ "mx-auto text-center" $ do
      h1 ! A.class_ "text-3xl mb-2" $ "Hipsterfy"
      p ! A.class_ "mt-2" $ "Which hipsters do you and your friends both follow?"
      div ! A.class_ "mt-6" $ do
        a
          ! A.class_ "py-2 px-4 text-sm font-medium rounded-md text-white"
          ! A.style "background-color: #1DB954;"
          ! A.href "/authorize"
          $ "Sign in with Spotify"

accountPage :: User -> (UpdateStatus, [(SpotifyArtist, Maybe SpotifyArtistInsights)]) -> LText
accountPage User {spotifyUserName, friendCode} (status, artists) = container $ do
  p $
    "Logged in as "
      <> toHtml spotifyUserName
      <> " "
      <> ( a ! A.class_ "underline text-blue-700"
             ! A.href "/logout"
             $ "(log out)"
         )
  p $ "Your friend code is: " <> code (toHtml friendCode)
  form ! A.action "/compare" ! A.method "POST" $ do
    br
    label ! A.class_ "block text-sm font-medium text-gray-700" $ "Friend code:"
    div ! A.class_ "mt-1 relative rounded-md shadow-sm" $
      input ! A.class_ "form-input block w-full" ! A.type_ "text" ! A.name "friend-code"
    p ! A.class_ "mt-2 text-sm text-gray-500" $ "Enter a friend code to see mutually followed hipsters."
    div ! A.class_ "mt-5"
      $ span ! A.class_ "inline-flex rounded-md shadow-sm"
      $ button
        ! A.class_ "inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md text-white"
        ! A.style "background-color: #1DB954;"
        ! A.type_ "submit"
      $ "Enter"
    p ! A.class_ "mt-2 text-sm text-gray-500 " $ "Comparison may take a bit. Loading artist listener counts is slow."
    h2 ! A.class_ "text-2xl mt-4 mb-2" $ "Artists you follow"
    case status of
      UpdatedAt lastUpdated -> p ! A.class_ "mb-2 text-sm text-gray-500 " $ "Last updated at " <> show lastUpdated <> "."
      InProgress finished total -> do
        p ! A.class_ "mb-2 text-sm text-gray-500 " $ "Some artists are still loading (" <> show finished <> " out of ~" <> show total <> " complete)."
        p ! A.class_ "mb-2 text-sm text-gray-500 " $ "Refresh for a more recent view."
    artistTable artists

comparePage :: [(SpotifyArtist, SpotifyArtistInsights)] -> [(SpotifyArtist, SpotifyArtistInsights)] -> LText
comparePage yourFollowedArtists friendFollowedArtists = container $ do
  p "Artists you both follow, in ascending order by listeners:"
  br
  artistTable $ second Just <$> intersect yourFollowedArtists friendFollowedArtists
  br
  a ! A.class_ "underline text-blue-700" ! A.href "/" $ "Go back"

artistTable :: [(SpotifyArtist, Maybe SpotifyArtistInsights)] -> Html
artistTable artists =
  table ! A.class_ "w-full" $ do
    thead
      $ tr
      $ do
        th ! A.class_ "font-medium text-left" $ "Artist"
        th ! A.class_ "font-medium text-right" $ "Monthly listeners"
    tbody $ mconcat $ fmap renderArtist $ sortHipster $ ordNub artists
  where
    sortHipster :: [(SpotifyArtist, Maybe SpotifyArtistInsights)] -> [(SpotifyArtist, Maybe SpotifyArtistInsights)]
    sortHipster = sortOn $ monthlyListeners . fromMaybe (SpotifyArtistInsights 0) . snd
    renderArtist :: (SpotifyArtist, Maybe SpotifyArtistInsights) -> Html
    renderArtist (SpotifyArtist {spotifyURL, name}, insights) = tr $ do
      td $ a ! A.class_ "underline text-blue-700" ! A.href (textValue spotifyURL) $ toHtml name
      td ! A.class_ "text-right" $ toHtml $ case insights of
        Just SpotifyArtistInsights {monthlyListeners} -> formatInt monthlyListeners
        Nothing -> "?"
    formatInt :: Int -> LText
    formatInt = toLText . reverse . intercalate "," . chunksOf 3 . reverse . show

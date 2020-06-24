module Hipsterfy.Pages (loginPage, accountPage, comparePage) where

import Data.List (intersect)
import Data.List.Split (chunksOf)
import Data.Map (toDescList)
import Hipsterfy.Artist (Artist (..))
import Hipsterfy.Spotify (SpotifyArtist (..))
import Hipsterfy.User (UpdateStatus (..), User (..))
import Relude hiding (div)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 hiding (body, contents, head)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

render :: Html -> LText
render body = renderHtml $ docTypeHtml $ do
  H.head $ do
    title "Hipsterfy"
    meta ! A.name "description" ! A.content "See which hipsters you and your friends both follow"
    meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
    link ! A.href "https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css" ! A.rel "stylesheet"
    link ! A.href "https://cdn.jsdelivr.net/npm/@tailwindcss/ui@latest/dist/tailwind-ui.min.css" ! A.rel "stylesheet"

  H.body $ do
    div ! A.class_ "flex py-8 px-4 sm:py-12" $ do
      div ! A.class_ "mx-auto max-w-lg" $ do
        body
    footer ! A.class_ "text-center text-sm pb-12"
      $ p
      $ a ! A.class_ "underline text-blue-700"
        ! A.target "_blank"
        ! A.href "https://github.com/liftM/hipsterfy"
      $ "About"

container :: Html -> LText
container contents = render $ do
  h1 ! A.class_ "text-3xl mb-2" $ "Hipsterfy"
  contents

loginPage :: LText
loginPage = render $ do
  div ! A.class_ "text-center" $ do
    h1 ! A.class_ "text-3xl mb-2" $ "Hipsterfy"
    p ! A.class_ "mt-2" $ "Which hipsters do you and your friends both follow?"
    div ! A.class_ "mt-6" $ do
      a
        ! A.class_ "py-2 px-4 text-sm font-medium rounded-md text-white"
        ! A.style "background-color: #1DB954;"
        ! A.href "/authorize"
        $ "Sign in with Spotify"

accountPage :: User -> UpdateStatus -> [Artist] -> LText
accountPage User {spotifyUserName, friendCode, lastUpdated} status artists = container $ do
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
    p ! A.class_ "mt-2 text-sm text-gray-500" $ "Enter a friend code to see mutually followed artists."
    div ! A.class_ "mt-5"
      $ span ! A.class_ "inline-flex rounded-md shadow-sm"
      $ button
        ! A.class_ "inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md text-white"
        ! A.style "background-color: #1DB954;"
        ! A.type_ "submit"
      $ "Enter"
    h2 ! A.class_ "text-2xl mt-12 mb-2" $ "Artists you follow"
    case status of
      QueuedAt _ -> loading
      _ -> case lastUpdated of
        Just t -> p ! A.class_ "mb-2 text-sm text-gray-500 " $ "Last updated at " <> show t <> "."
        Nothing -> loading
    artistTable artists
  where
    loading = p ! A.class_ "mb-2 text-sm text-gray-500 " $ "Some artists are still loading. Refresh for a more recent view."

comparePage :: [Artist] -> [Artist] -> LText
comparePage yourFollowedArtists friendFollowedArtists = container $ do
  p "Artists you both follow, in ascending order by listeners:"
  br
  artistTable $ intersect yourFollowedArtists friendFollowedArtists
  br
  a ! A.class_ "underline text-blue-700" ! A.href "/" $ "Go back"

artistTable :: [Artist] -> Html
artistTable artists =
  table ! A.class_ "w-full" $ do
    thead
      $ tr
      $ do
        th ! A.class_ "font-medium text-left align-top" $ "Artist"
        th ! A.class_ "font-medium text-right align-top" ! A.style "min-width: 8rem" $ "Monthly listeners"
    tbody $ mconcat $ fmap renderArtist $ sortHipster $ ordNub artists
  where
    sortHipster :: [Artist] -> [Artist]
    sortHipster = sortOn $ fromMaybe 0 . listeners . monthlyListeners
    renderArtist :: Artist -> Html
    renderArtist Artist {spotifyArtist = SpotifyArtist {spotifyURL, name}, monthlyListeners} =
      tr $ do
        td $ a ! A.class_ "underline text-blue-700" ! A.href (textValue spotifyURL) $ toHtml name
        td ! A.class_ "text-right" $ toHtml $ maybe "?" formatInt (listeners monthlyListeners)
    formatInt :: Int -> LText
    formatInt = toLText . reverse . intercalate "," . chunksOf 3 . reverse . show
    listeners :: Map t Int -> Maybe Int
    listeners m = fmap snd $ viaNonEmpty head $ toDescList m

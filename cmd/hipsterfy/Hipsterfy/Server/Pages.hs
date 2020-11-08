module Hipsterfy.Server.Pages (loginPage, accountPage, comparePage, PageError(FriendCodeError)) where
import qualified Data.HashMap.Strict as HashMap
import Data.List (intersect)
import Data.List.Split (chunksOf)
import Data.Time (Day)
import Hipsterfy.Artist (Artist (..))
import Hipsterfy.Jobs (UpdateJobInfo (..), UpdateStatus (..))
import Hipsterfy.Spotify (SpotifyArtist (..))
import Hipsterfy.User (User (..))
import Relude hiding (div)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 hiding (body, contents, head, map)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data PageError = FriendCodeError

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
      $ p $ do
        "Hipsterfy is "
        externalHref "https://github.com/liftM/hipsterfy" "open source"
        "."

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
accountPage User {updateJobInfo = UpdateJobInfo {..}, ..} status artists = container $ do
  p $
    "Logged in as "
      <> toHtml spotifyUserName
      <> " "
      <> href "/logout" "(log out)"
  p $ "Your friend code is: " <> code (toHtml friendCode)
  form ! A.action "/compare" ! A.method "POST" $ do
    br
    label ! A.class_ "block text-sm font-medium text-gray-700" $ "Friend code:"
    div ! A.class_ "mt-1 mb-2 relative rounded-md shadow-sm" $
      input ! A.class_ "form-input block w-full" ! A.type_ "text" ! A.name "friend-code"
    greyed "Enter a friend code to see mutually followed artists."
    div ! A.class_ "mt-5"
      $ span ! A.class_ "inline-flex rounded-md shadow-sm"
      $ button
        ! A.class_ "inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md text-white"
        ! A.style "background-color: #1DB954;"
        ! A.type_ "submit"
      $ "Enter"
    h2 ! A.class_ "text-2xl mt-12 mb-2" $ "Artists you follow"
    case status of
      QueuedAt _ -> loadingArtists
      _ -> case lastUpdateJobCompleted of
        Just t ->
          greyed $ do
            "Followed artists last updated at " <> show t <> ". "
            br
            loadingListeners artists
            href "/refresh" "Check for new followed artists."
        Nothing -> loadingArtists
    artistTable artists

comparePage :: Either PageError ((UpdateStatus, [Artist]), (UpdateStatus, [Artist])) -> LText
comparePage errorOrCompare = container $ do
  case errorOrCompare of
    Left FriendCodeError ->
      do div ! A.class_ "self-center mt-5 text-red-700" $ "Error: This is an invalid friend code."
    Right ((x, xs),(y, ys)) -> do
        p "Artists you both follow, in ascending order by listeners:"
        case x of
          QueuedAt _ -> loadingArtists
          _ -> case y of
            QueuedAt _ -> loadingArtists
            _ -> pass
        greyed $ loadingListeners $ hashNub $ xs <> ys
        br
        artistTable $ intersect xs ys
        br
        href "/" "Go back"

artistTable :: [Artist] -> Html
artistTable artists =
  table ! A.class_ "w-full" $ do
    thead
      $ tr
      $ do
        th ! A.class_ "font-medium text-left align-top" $ "Artist"
        th ! A.class_ "font-medium text-right align-top" ! A.style "min-width: 8rem" $ "Monthly listeners"
    tbody $ mconcat $ fmap renderArtist $ sortHipster $ hashNub artists
  where
    sortHipster :: [Artist] -> [Artist]
    sortHipster = sortOn $ fromMaybe 0 . listeners . monthlyListeners
    renderArtist :: Artist -> Html
    renderArtist Artist {spotifyArtist = SpotifyArtist {spotifyURL, name}, monthlyListeners} =
      tr $ do
        td $ externalHref spotifyURL $ toHtml name
        td ! A.class_ "text-right" $ toHtml $ maybe "?" formatInt (listeners monthlyListeners)
    formatInt :: Int -> LText
    formatInt = toLText . reverse . intercalate "," . chunksOf 3 . reverse . show
    listeners :: HashMap Day Int -> Maybe Int
    listeners m = fmap snd $ viaNonEmpty head $ reverse $ sort $ HashMap.toList m

loadingArtists :: Html
loadingArtists = greyed $ do
  "Checking for more followed artists. This can take a couple minutes."
  br
  "Refresh for a more recent view."

loadingListeners :: [Artist] -> Html
loadingListeners artists =
  when (any (null . monthlyListeners) artists) $ do
    "Some listener counts are still loading (" <> show numComplete <> " / " <> show (length artists) <> " completed)."
    br
  where
    numComplete = length $ filter id $ map (not . null . monthlyListeners) artists

href :: Text -> Html -> Html
href url = a ! A.class_ "underline text-blue-700" ! A.href (textValue url)

externalHref :: Text -> Html -> Html
externalHref url = href url ! A.target "_blank"

greyed :: Html -> Html
greyed = p ! A.class_ "mb-2 text-sm text-gray-500"

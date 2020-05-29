module Hipsterfy.Pages (loginPage, accountPage, comparePage) where

import qualified Data.Text.Lazy as LT
import Hipsterfy.User (User (..))
import Relude
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!), Html, docTypeHtml, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

render :: Html -> LT.Text
render body = renderHtml $ docTypeHtml $ do
  H.head $ H.title "Hipsterfy"
  H.body body

loginPage :: LT.Text
loginPage = render $ do
  H.a ! A.href "/authorize" $ "Authorize with Spotify"

accountPage :: User -> LT.Text
accountPage user = render $ do
  H.p $ "Logged in as " `mappend` toHtml (spotifyUserID user)
  H.form ! A.action "/compare" ! A.method "POST" $ do
    H.label "Add friend code:"
    H.input ! A.type_ "text" ! A.name "friend-code"
    H.input ! A.type_ "submit"
  H.a ! A.href "/logout" $ "Log out"

comparePage :: LT.Text
comparePage = render $ do
  H.p "comparison page placeholder"

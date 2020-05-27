module Hipsterfy.Pages (loginPage, accountPage) where

import Relude
import qualified Data.Text.Lazy as LT
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!), Html, docTypeHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

render :: Html -> LT.Text
render body = renderHtml $ docTypeHtml $ do
  H.head $ H.title $ "Hipsterfy"
  H.body body

loginPage :: LT.Text
loginPage = render $ do
  H.a ! A.href "/authorize" $ "Authorize with Spotify"

accountPage :: LT.Text
accountPage = render $ do
  H.p "Logged in"
  H.a ! A.href "/logout" $ "Log out"

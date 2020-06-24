module Hipsterfy.Spotify.Spec (testGetAlbums) where

import Hipsterfy.Spotify (getSpotifyArtistsOfSavedTracks, getSpotifyArtistsOfSavedAlbums)
import Hipsterfy.Spotify.Auth (SpotifyCredentials (..))
import Relude
import Test.Hspec (Spec, shouldSatisfy, describe, it)

testGetAlbums :: SpotifyCredentials -> Spec
testGetAlbums creds = do
  describe "Hipsterfy.Spotify" $ do
    describe "getSpotifyArtistsOfSavedAlbums" $ do
      it "does not throw" $ do
        artists <- getSpotifyArtistsOfSavedAlbums creds
        artists `shouldSatisfy` const True

    describe "getSpotifyArtistsOfSavedTracks" $ do
      it "does not throw" $ do
        artists <- getSpotifyArtistsOfSavedTracks creds
        artists `shouldSatisfy` const True

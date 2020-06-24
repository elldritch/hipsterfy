module Hipsterfy.Spotify.Spec (testGetAlbums) where

import Hipsterfy.Spotify (getSpotifyArtistsOfSavedAlbums, getSpotifyArtistsOfSavedTracks)
import Hipsterfy.Spotify.Auth (SpotifyCredentials (..))
import Relude
import Test.Hspec (Spec, describe, it, shouldSatisfy)

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

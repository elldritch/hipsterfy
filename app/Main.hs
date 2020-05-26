module Main (main) where

import Relude

import Options.Applicative
  ( ParserInfo,
    auto,
    briefDesc,
    execParser,
    helper,
    info,
    long,
    option,
    progDesc,
    short,
    strOption,
  )

import Hipsterfy (runServer, Options(Options))

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (briefDesc <> progDesc "Hipsterfy server")
  where
    options =
      Options
        <$> strOption (long "host")
        <*> option auto (long "port" <> short 'p')
        <*> strOption (long "db")
        <*> strOption (long "client_id")
        <*> strOption (long "client_secret")

main :: IO ()
main = do
  options <- execParser opts
  runServer options

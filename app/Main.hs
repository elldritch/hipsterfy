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

import Hipsterfy (runServer, ServerOptions(ServerOptions))

data Options = Options
  { port :: Int,
    pgConn :: Text
  }
  deriving (Show)

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (briefDesc <> progDesc "Hipsterfy server")
  where
    options =
      Options
        <$> option auto (long "port" <> short 'p')
        <*> strOption (long "db")

main :: IO ()
main = do
  options <- execParser opts
  print options
  runServer $ ServerOptions (port options) (encodeUtf8 $ pgConn options)

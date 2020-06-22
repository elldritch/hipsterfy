module Main (main) where

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
import Relude
import System.Environment (getArgs)

data TestOptions = TestOptions {}

opts :: ParserInfo TestOptions
opts =
  info
    (options <**> helper)
    (briefDesc <> progDesc "Hipsterfy server")
  where
    options =
      TestOptions
        <$> strOption (long "host")
        <*> option auto (long "port" <> short 'p')
        <*> strOption (long "db")
        <*> strOption (long "client_id")
        <*> strOption (long "client_secret")

-- TODO: add automated integration tests, where authenticated is passed in with
-- flags.

main :: IO ()
main = do
  args <- getArgs
  putStrLn "args"
  print args
  options <- execParser opts
  putStrLn "not a test (yet)"

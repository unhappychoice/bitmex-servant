# Auto-Generated Swagger Bindings to `BitMEX`

The library in `lib` provides auto-generated-from-Swagger bindings to the BitMEX API.

## Installation

Installation follows the standard approach to installing Stack-based projects.

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack install` to install this package.

## Main Interface

The main interface to this library is in the `BitMEX.API` module, which exports the BitMEXBackend type. The BitMEXBackend
type can be used to create and define servers and clients for the API.

## Creating a Client

A client can be created via the `createBitMEXClient` function, which, if provided with a hostname and a port, will generate
a client that can be used to access the API if it is being served at that hostname / port combination. For example, if
`localhost:8080` is serving the BitMEX API, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import BitMEX.API

main :: IO ()
main = do
  BitMEXBackend{..} <- createBitMEXClient (ServerConfig "localhost" 8080)
  -- Any BitMEX API call can go here.
  return ()
```

## Creating a Server

In order to create a server, you must use the `runBitMEXServer` function. However, you unlike the client, in which case you *got* a `BitMEXBackend`
from the library, you must instead *provide* a `BitMEXBackend`. For example, if you have defined handler functions for all the
functions in `BitMEX.Handlers`, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import BitMEX.API

-- A module you wrote yourself, containing all handlers needed for the BitMEXBackend type.
import BitMEX.Handlers

-- Run a BitMEX server on localhost:8080
main :: IO ()
main = do
  let server = BitMEXBackend{..}
  runBitMEXServer (ServerConfig "localhost" 8080) server
```

You could use `optparse-applicative` or a similar library to read the host and port from command-line arguments:
```
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import BitMEX.API (runBitMEXServer, BitMEXBackend(..), ServerConfig(..))

import Control.Applicative ((<$>), (<*>))
import Options.Applicative (execParser, option, str, auto, long, metavar, help)

main :: IO ()
main = do
  config <- parseArguments
  runBitMEXServer config BitMEXBackend{}

-- | Parse host and port from the command line arguments.
parseArguments :: IO ServerConfig
parseArguments =
  execParser $
    ServerConfig
      <$> option str  (long "host" <> metavar "HOST" <> help "Host to serve on")
      <*> option auto (long "port" <> metavar "PORT" <> help "Port to serve on")
```

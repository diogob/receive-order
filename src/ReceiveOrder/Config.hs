module ReceiveOrder.Config ( prettyVersion
                           , readOptions
                           , AppConfig (..)
                           ) where

import           Data.Text hiding (concat)
import Data.Monoid
import           Data.Version                (versionBranch)
import           Options.Applicative
import           Options.Applicative.Text
import           Paths_receive_order          (version)
import Network.Wai.Handler.Warp (Port)

-- | Data type to store all command line options
data AppConfig = AppConfig
  { configDatabase  :: Text
  , port :: Port
  }

argParser :: Parser AppConfig
argParser = AppConfig
  <$> argument text (help "(REQUIRED) database connection string, e.g. postgres://user:pass@host:port/db" <> metavar "DB_URL")
  <*> option auto (long "port" <> short 'p' <> help "port number on which to run HTTP server" <> metavar "PORT" <> value 8080 <> showDefault)

-- | User friendly version number
prettyVersion :: String
prettyVersion = concat $ show <$> versionBranch version

-- | Function to read and parse options from the command line
readOptions :: IO AppConfig
readOptions = customExecParser parserPrefs opts
  where
    opts = info (helper <*> argParser) $
                    fullDesc
                    <> (progDesc) (
                    ("receive-order-api ")
                    <> prettyVersion
                    <> (" / Serves a RESTful API for Receive Orders")
                    )
    parserPrefs = prefs showHelpOnError

module Main where

---------------------------------------------------------------------------------
import Options.Applicative
---------------------------------------------------------------------------------
import App (exec)
import Config (AppConfig(..))
---------------------------------------------------------------------------------

appConfig :: Parser AppConfig
appConfig = AppConfig
  <$> strOption
      ( long "config"
     <> help "Path to configuration file"
     <> metavar "CONFIG"
     <> showDefault
     <> value "./haskell-web-app-freer.cfg"
      )
  <*> switch
      ( long "check"
     <> help "Check if configuration is sane without running app"
      )

opts :: ParserInfo AppConfig
opts =
  info appConfig
    ( fullDesc
   <> progDesc "haskell-web-app-freer API"
   <> header "backend - haskell-web-app-freer API"
    )

main :: IO ()
main = execParser opts >>= \ac -> exec ac

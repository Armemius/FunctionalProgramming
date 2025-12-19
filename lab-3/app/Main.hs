{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Lib
import Options.Applicative
import System.IO (BufferMode (..), hGetLine, hIsEOF, hPutStrLn, hSetBuffering, stderr, stdin, stdout)
import Text.Read (readMaybe)
import System.Exit (exitFailure)

data Options = Options
  { optStep :: Double
  , optLinear :: Bool
  , optNewton :: Bool
  , optLagrange :: Bool
  , optWindow :: Int
  }

main :: IO ()
main = do
  Options {optStep, optLinear, optNewton, optLagrange, optWindow} <- execParser opts
  let requestedAlgs =
        (if optLinear then [Linear] else [])
          ++ (if optNewton then [Newton optWindow] else [])
          ++ (if optLagrange then [Lagrange optWindow] else [])
      algorithms =
        if null requestedAlgs
          then [Linear]
          else requestedAlgs
      cfg = Config {cfgStep = optStep, cfgAlgorithms = algorithms}
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  finalState <- loop cfg (initialState cfg)
  mapM_ (putStrLn . renderOutput) (finalizeOutputs cfg finalState)
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Stream interpolation (linear and Newton) driven by stdin"
        )

loop :: Config -> State -> IO State
loop cfg state = do
  done <- hIsEOF stdin
  if done
    then pure state
    else do
      line <- hGetLine stdin
      case parsePoint line of
        Nothing -> do
          hPutStrLn stderr ("Skipping malformed line: " <> line)
          loop cfg state
        Just pt -> do
          case pushPoint cfg state pt of
            Left err -> hPutStrLn stderr err >> exitFailure
            Right (state', outputs) -> do
              mapM_ (putStrLn . renderOutput) outputs
              loop cfg state'

optionsParser :: Parser Options
optionsParser =
  Options
    <$> option
      (eitherReader positiveDouble)
      ( long "step"
          <> short 's'
          <> help "Sampling step for generated X values"
          <> metavar "STEP"
          <> value 1.0
      )
    <*> switch
      ( long "linear"
          <> help "Enable linear interpolation (default when nothing is chosen)"
      )
    <*> switch
      ( long "newton"
          <> help "Enable Newton interpolation"
      )
    <*> switch
      ( long "lagrange"
          <> help "Enable Lagrange interpolation"
      )
    <*> option
      (eitherReader positiveWindow)
      ( long "points"
          <> short 'n'
          <> help "Number of points in the sliding window for Newton interpolation"
          <> metavar "N"
          <> value 4
      )

positiveDouble :: String -> Either String Double
positiveDouble raw =
  case readMaybe raw of
    Just v | v > 0 -> Right v
    _ -> Left "step must be a positive number"

positiveWindow :: String -> Either String Int
positiveWindow raw =
  case readMaybe raw of
    Just v | v >= 2 -> Right v
    _ -> Left "window size must be at least 2"

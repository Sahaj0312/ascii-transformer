module AsciiConverter.ExtractFrames (extractFrames, hasFFmpeg) where
import System.Process()
import System.Exit
import System.Process

hasFFmpeg :: IO Bool
hasFFmpeg = do
  (_, _, _, processHandle) <- createProcess (proc "ffmpeg" ["-version"])
  exitCode <- waitForProcess processHandle
  return (exitCode == ExitSuccess)

-- Extracts frames from a video file using ffmpeg
extractFrames :: FilePath -> IO ()
extractFrames file = do
  let cmd = "ffmpeg -i " ++ file ++ " -vf fps=10 " ++ file ++ "-frame-%04d.jpg"
  (code, out, err) <- readProcessWithExitCode "sh" ["-c", cmd] ""
  putStrLn out
  putStrLn err


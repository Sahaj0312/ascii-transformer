module AsciiConverter.ExtractFrames (extractFrames, hasFFmpeg) where
import System.Process()
import System.Exit
import System.Process
import System.FilePath (dropExtension)

hasFFmpeg :: IO Bool
hasFFmpeg = do
  (_, _, _, processHandle) <- createProcess (proc "ffmpeg" ["-version"])
  exitCode <- waitForProcess processHandle
  return (exitCode == ExitSuccess)

-- Extracts frames from a video file using ffmpeg
extractFrames :: FilePath -> IO ()
extractFrames inputFilePath = do
  let outputFilePath = dropExtension inputFilePath ++ "-frame-%04d.jpg"
  let cmd = "ffmpeg -i " ++ inputFilePath ++ " -vf fps=30 " ++ outputFilePath
  (_, out, err) <- readProcessWithExitCode "sh" ["-c", cmd] ""
  putStrLn out
  putStrLn err



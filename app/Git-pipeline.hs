import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

main :: IO ()
main = do
    -- Run `cabal run` and capture the exit code and output
    (exitCode, output, _) <- readProcessWithExitCode "cabal" ["run"] ""

    -- Check if the exit code is successful (ExitSuccess) and there are no error messages
    if exitCode == ExitSuccess && null output
        then putStrLn "Test passed: Program compiled and ran successfully."
        else putStrLn $ "Test failed: " ++ show exitCode ++ "\n" ++ output

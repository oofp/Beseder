module ProcessExample where

import System.Process 
import qualified System.IO as SIO
import Protolude

test :: IO ()
test = do
    (inn, out, err, idd) <- runInteractiveCommand "cmd"    
    mapM_ (flip SIO.hSetBinaryMode False) [inn, out]             
    SIO.hSetBuffering inn SIO.LineBuffering                          
    SIO.hSetBuffering out SIO.NoBuffering
    echoIt inn out

echoIt inn out = do
    hPutStrLn inn ("enter something" :: Text)
    line <- SIO.hGetLine out
    hPutStrLn inn line
    unless (line == "q") (echoIt  inn out)



test2 :: IO ()
test2 = do
    (inn, out, err, idd) <- runInteractiveCommand "inp_par.bat test"    
    mapM_ (flip SIO.hSetBinaryMode False) [inn, out]             
    SIO.hSetBuffering inn SIO.LineBuffering                          
    SIO.hSetBuffering out SIO.NoBuffering
    line <- SIO.hGetLine out
    hPutStrLn inn line


--
-- code for a maxima client
--
-- mjsottile\@computer.org
-- 

module GEP.Examples.Regression.MaximaClient (
    maximaExpand,
    maximaExpandUnsafe
) where

import System.IO.Unsafe
import System.IO
import Network

readToEof :: Handle -> IO [String]
readToEof h = 
  do checkEOF <- hIsEOF h
     (if checkEOF
      then return []
      else do line <- hGetLine h
              rest <- readToEof h
              return $ line:rest)

maximaCommand :: String -> HostName -> PortNumber -> IO [String]
maximaCommand s host port =
    do h <- connectTo host (PortNumber port)
       -- NOTE: first blank line is important.  the simple protocol
       -- the server assumes is that a blank line will be sent and
       -- then a single line with the maxima command without a
       -- trailing semicolon.  If a nonempty line comes first, the
       -- server assumes it is an imposter and kills itself.
       hPutStrLn h ""
       hPutStrLn h s
       hSetBuffering h LineBuffering
       vals <- readToEof h
       hClose h
       return vals

maximaExpand :: String -> String -> PortNumber -> IO [String]
maximaExpand s host port = 
  do vals <- maximaCommand ("expand("++s++")") host port
     return $ vals

maximaExpandUnsafe :: String -> String -> PortNumber -> [String]
maximaExpandUnsafe s host port =
    unsafePerformIO (maximaExpand s host port)

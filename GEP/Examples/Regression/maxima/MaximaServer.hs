--
-- simple program to accept commands to send to maxima and return the
-- result.
--
-- mjsottile\@computer.org
--

import Network
import System.IO
import System.Cmd
import System.Process
import System.Directory

getNLines 0 h = return []
getNLines n h = 
  do checkEOF <- hIsEOF h
     (if checkEOF
      then return []
      else do line <- hGetLine h
              lines <- getNLines (n-1) h
              return $ line:lines)


readToPrefix h p = 
  do checkEOF <- hIsEOF h
     (if checkEOF
      then return []
      else 
          do line <- hGetLine h
             (if p == (take (length p) line)
              then return [line]
              else do lines <- readToPrefix h p
                      return $ line:lines)
             )

readToEof h = 
  do checkEOF <- hIsEOF h
     (if checkEOF
      then return []
      else do line <- hGetLine h
              lines <- readToEof h
              return $ line:lines)

invokeMaxima str = 
  do cmdFile <- openFile "command.lisp" WriteMode
     hPutStrLn cmdFile (str++"$")
     hPutStrLn cmdFile "%;"
     hClose cmdFile
     (Just stdIn, Just stdOut, Just stdErr, handle) <- createProcess (proc
         "maxima" ["-b","command.lisp","-q"])
         {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe} 
     hSetBuffering stdOut LineBuffering
     junk <- readToPrefix stdOut "(%i3)"
     theOutput <- readToEof stdOut
     hClose stdIn
     hClose stdOut
     hClose stdErr
     terminateProcess handle
     removeFile "command.lisp"
     return theOutput

serverloop sock = 
    do (h,host,port) <- accept sock
       hSetBuffering h LineBuffering
       junk <- hGetLine h
       (if (length junk) > 1
        then hClose h
        else 
            do theString <- hGetLine h
               ts <- invokeMaxima theString
               mapM (\i -> hPutStrLn h i) ts
               hClose h
               serverloop sock)

server port = do sock <- listenOn (PortNumber port)
                 serverloop sock
                 sClose sock

main = do server 12777

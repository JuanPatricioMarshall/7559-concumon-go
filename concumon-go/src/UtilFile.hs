module UtilFile
    ( UtilFile.getParameter,
      UtilFile.fileToList
    ) where


getParameter :: [(String,Int)] -> String -> Int -> Int
getParameter fileList parameterName defaultValue = do
  let parameter = (head (filter ((==parameterName).fst) fileList))
  if null parameter
    then defaultValue
    else snd parameter

fileToList :: String -> IO [(String,Int)]    
fileToList = readIO
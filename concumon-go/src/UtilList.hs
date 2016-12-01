module UtilList
    ( UtilList.safeReplaceElement,
      UtilList.getIndexOfFirstBoolEqualTo,
      UtilList.getIndexOfFirstIntEqualTo,
      UtilList.updateConcurrentList
    ) where

import Control.Monad
import Control.Concurrent


updateConcurrentList :: MVar([a]) -> Int -> a -> IO()
updateConcurrentList mVar index value = do
	list <- takeMVar mVar
	let newList = UtilList.safeReplaceElement list index value
	putMVar mVar newList

getIndexOfFirstBoolEqualTo :: [Bool] ->  Bool ->  Int
getIndexOfFirstBoolEqualTo list value = do
	let listSize = length list
	let indexList = take listSize (iterate (1+) 0)
	let auxList = zip indexList list
	let filterList = filter ((==value).snd) auxList
	
	if null filterList 
		then -1
		else	
			fst (head filterList)



getIndexOfFirstIntEqualTo :: [Int] ->  Int ->  Int
getIndexOfFirstIntEqualTo list value = do
	let listSize = length list
	let indexList = take listSize (iterate (1+) 0)
	let auxList = zip indexList list
	let filterList = filter ((==value).snd) auxList
	
	if null filterList 
		then -1
		else	
			fst (head filterList)
		
-- | Replaces an element in a list with a new element, if that element exists.
safeReplaceElement
  -- | The list
  :: [a]
  -- | Index of the element to replace.
  -> Int
  -- | The new element.
  -> a
  -- | The updated list.
  -> [a]
safeReplaceElement xs i x =
  if i >= 0 && i < length xs
    then replaceElement xs i x
    else xs


-- | Replaces an element in a list with a new element.
replaceElement
  -- | The list
  :: [a]
  -- | Index of the element to replace.
  -> Int
  -- | The new element.
  -> a
  -- | The updated list.
  -> [a]
replaceElement xs i x = fore ++ (x : aft)
  where fore = take i xs
        aft = drop (i+1) xs

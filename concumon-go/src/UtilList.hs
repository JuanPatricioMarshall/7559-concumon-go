module UtilList
    ( UtilList.safeReplaceElement,
      UtilList.getIndexOfFirstBoolEqualTo,
      UtilList.getIndexOfFirstIntEqualTo,
      UtilList.updateConcurrentList,
      UtilList.getAdyacents,
      UtilList.getAdyacentPosition
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

getAdyacents :: Integer -> Integer -> Integer -> IO ()
getAdyacents position rows columns = do
	let list = []
	let rowPos = position `div` rows
	let colPos = position `mod` columns

	--topLeft 
	let i = rowPos - 1
	let j = colPos - 1

	let topLeft = getAdyacentPosition i j rows columns
	when (topLeft >= 0) $ putStrLn (show(topLeft))
	
	--top
	let i = rowPos - 1
	let j = colPos

	let top = getAdyacentPosition i j rows columns
	when (top >= 0) $ putStrLn (show(top))

	--topRigth
	let i = rowPos - 1
	let j = colPos + 1

	let topRigth = getAdyacentPosition i j rows columns
	when (topRigth >= 0) $ putStrLn (show(topRigth))

	--middleLeft
	let i = rowPos
	let j = colPos - 1

	let middleLeft = getAdyacentPosition i j rows columns
	when (middleLeft >= 0) $ putStrLn (show(middleLeft))

	--middleRigth
	let i = rowPos
	let j = colPos + 1

	let middleRigth = getAdyacentPosition i j rows columns
	when (middleRigth >= 0) $ putStrLn (show(middleRigth))
	
	--bottomLeft
	let i = rowPos + 1
	let j = colPos - 1

	let bottomLeft = getAdyacentPosition i j rows columns
	when (bottomLeft >= 0) $ putStrLn (show(bottomLeft))

	--bottom
	let i = rowPos + 1
	let j = colPos

	let bottom = getAdyacentPosition i j rows columns
	when (bottom >= 0) $ putStrLn (show(bottom))

	--bottomRight
	let i = rowPos + 1
	let j = colPos + 1

	let bottomRight = getAdyacentPosition i j rows columns
	when (bottomRight >= 0) $ putStrLn (show(bottomRight))


getAdyacentPosition :: Integer -> Integer -> Integer -> Integer -> Integer
getAdyacentPosition i j rows columns =
	if i >= 0 && j >=0 && i < rows && j < columns
	 then i * columns + j
	else
	 -1
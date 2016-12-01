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

getAdyacents :: Int -> Int -> Int -> [Int]
getAdyacents position rows columns = do
	let list = [-1,-1,-1,-1,-1,-1,-1,-1]
	let rowPos = position `div` rows
	let colPos = position `mod` columns

	--topLeft 
	let i = rowPos - 1
	let j = colPos - 1

	let topLeft = getAdyacentPosition i j rows columns	
	let list1 = safeReplaceElement list 0 topLeft
	--when (topLeft >= 0) $ putStrLn (show(topLeft))

	--top
	let i = rowPos - 1
	let j = colPos

	let top = getAdyacentPosition i j rows columns
	let list2 = safeReplaceElement list1 1 top

	--topRigth
	let i = rowPos - 1
	let j = colPos + 1

	let topRigth = getAdyacentPosition i j rows columns
	let list3 = safeReplaceElement list2 2 topRigth

	--middleLeft
	let i = rowPos
	let j = colPos - 1

	let middleLeft = getAdyacentPosition i j rows columns
	let list4 = safeReplaceElement list3 3 middleLeft

	--middleRigth
	let i = rowPos
	let j = colPos + 1

	let middleRigth = getAdyacentPosition i j rows columns
	let list5 = safeReplaceElement list4 4 middleRigth
	
	--bottomLeft
	let i = rowPos + 1
	let j = colPos - 1

	let bottomLeft = getAdyacentPosition i j rows columns
	let list6 = safeReplaceElement list5 5 bottomLeft

	--bottom
	let i = rowPos + 1
	let j = colPos

	let bottom = getAdyacentPosition i j rows columns
	let list7 = safeReplaceElement list6 6 bottom

	--bottomRight
	let i = rowPos + 1
	let j = colPos + 1

	let bottomRight = getAdyacentPosition i j rows columns
	let list8 = safeReplaceElement list7 7 bottomRight

	let filterList = filter (>=0) list8	
	if null filterList
		then
			[]
		else
			filterList



getAdyacentPosition :: Int -> Int -> Int -> Int -> Int
getAdyacentPosition i j rows columns =
	if i >= 0 && j >=0 && i < rows && j < columns
	 then i * columns + j
	else
	 -1
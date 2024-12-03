
whatToDo :: (Ord a, Num a) => a -> [Char]
whatToDo tempFa = if tempFa > 25
                  then "Hot"
                  else if tempFa < 15
                       then "Cold"
                       else "Warm"


{- 
specialBirthday :: (Eq a, Num a) => a -> [Char]
specialBirthday age =
  if age == 1
    then "First birthday!"
    else
      if age == 18
        then "You're an adult!"
        else
          if age == 60
            then "Finally, I can stop caring about new slang!"
            else "Nothing special"
-}


specialBirthday :: (Eq a, Num a) => a -> [Char]
specialBirthday 1 = "First birthday!"
specialBirthday 18 = "You're an adult!"
specialBirthday 60 = "Finally, I can stop caring about new slang!"
specialBirthday _ = "Nothing special"

fToC :: Fractional a => a -> a
fToC x =
    (x - 32) * 5 / 9

generateTxId :: String -> String -> Int -> String
generateTxId from to value = from ++ to ++ show value

createTx :: String -> String -> Int -> (String, String, Int, String)
createTx from to value = (from, to, value, generateTxId from to value)

generateBlockId :: String -> Int -> String
generateBlockId previousBlockId numTransactions = previousBlockId ++ show numTransactions

createBlock :: String -> [(String, String, Int, String)] -> (String, [(String, String, Int, String)], String)
createBlock prevBlockHash listTxs = (prevBlockHash, listTxs, generateBlockId prevBlockHash (length listTxs))

verifyBlock :: (String, [(String, String, Int, String)], String) -> String -> [(String, String, Int, String)] -> String
verifyBlock block prevBlockHash listTxs = if block == createBlock prevBlockHash listTxs
                                           then "Block is valid"
                                           else "Block is invalid"

whatsIsideThisList :: Show a => [a] -> [Char]
whatsIsideThisList [] = "Empty list"
whatsIsideThisList [x] = "This list has only one element: " ++ show x
whatsIsideThisList (x:xs) = "First element is " ++ show x ++ " and the rest is " ++ show xs

sumOfMoney :: (String, Int) -> (String, Int) -> String
sumOfMoney (name1, amount1) (name2, amount2) = "Between Daniel and Santiago there have " ++ show (amount1 + amount2) ++ " dollars"

--transferMoney :: [(String, Int)] -> Int -> [(String, Int)]
transferMoney :: (Num a, Ord a) =>[(String, a)] -> a -> [(String, a)]
--transferMoney [(name1, amount1), (name2, amount2)] amount = [(name1, amount1 - amount), (name2, amount2 + amount)]
--transferMoney [(name1, amount1), (name2, amount2), (name3, amount3)] amount = [(name1, amount1 - amount), (name2, amount2 - amount), (name3, amount3 +amount)]
--Update trasfermoney to make sure that no one could transfer more money than they have in their account
transferMoney [(name1, amount1), (name2, amount2), (name3, amount3)] amount
    | amount1 < amount = error "Not enough money"
    | amount2 < amount = error "Not enough money"
    | amount3 < amount = error "Not enough money"
    | otherwise = [(name1, amount1 - amount), (name2, amount2 - amount), (name3, amount3 + amount)]


--this is the catch exception!
transferMoney list _ = list


main :: IO ()
main = do
    let tx1 = createTx "12adsas3dasd" "asdasd45sadsasd6" 100
    let replicatedTxs = replicate 7 tx1
    let numberTxs = length replicatedTxs

    let block1 = createBlock "12asd34sad56" replicatedTxs

    let block1Hash = verifyBlock block1 "12asd34sad56" replicatedTxs

    let sumOfMoneyTest = sumOfMoney ("Daniel", 100) ("Santiago", 200)

    let transferMoneyTest = transferMoney [("Daniel", 200), ("Santiago", 200), ("Pantiago", 500)] 250 

    print block1
    print block1Hash
    print sumOfMoneyTest
    --print $ whatsIsideThisList [1, 2, 3, 4, 5]
    print transferMoneyTest
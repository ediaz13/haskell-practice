whatToDo tempFa = if tempFa > 25
                  then "Hot"
                  else if tempFa < 15
                       then "Cold"
                       else "Warm"



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

main :: IO ()
main = do
    let tx1 = createTx "12adsas3dasd" "asdasd45sadsasd6" 100
    let replicatedTxs = replicate 7 tx1
    let numberTxs = length replicatedTxs

    let block1 = createBlock "12asd34sad56" replicatedTxs

    print block1
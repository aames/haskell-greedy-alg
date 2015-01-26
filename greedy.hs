data Item = Item {
                weight::Double,
                profit::Double,
                item::String,
                efficiency::Double
                }
instance Show Item where
    show (Item weight profit item efficiency) = concat [item, " Weight: ", show weight, " Profit: ", show profit, " Efficiency: ", show efficiency]
-- items ::[Item]
-- items = [
        -- Item "Weapon and Ammunition" 4.13 1.4 0.0,
        -- Item "Water" 2.13 2.74 0.0,
        -- Item "Pith Helmet" 3.03 1.55 0.0,
        -- Item "Sun Cream" 2.26 0.82 0.0,
        -- Item "Tent" 3.69 2.38 0.0,
        -- Item "Flare Gun" 3.45 2.93 0.0,
        -- Item "Olive Oil" 1.09 1.77 0.0,
        -- Item "Firewood" 2.89 0.53 0.0,
        -- Item "Kendal Mint Cake" 1.08 2.77 0.0,
        -- Item "Snake Repellent Spray" 3.23 4.29 0.0,
        -- Item "Bread" 2.29 2.85 0.0,
        -- Item "Pot Noodles" 0.55 0.34 0.0,
        -- Item "Software Engineering Textbook" 2.82 (-0.45) 0.0,
        -- Item "Tinned food" 2.31 2.17 0.0,
        -- Item "Pork Pie" 1.63 1.62 0.0
    -- ]

    
main = do
    contents <- readFile "items.txt"
    let lines = splitString contents '\n'
    let i = [ createItem x | x <- lines ]
    let items = qsort $ calculateEfficiency i
    putStrLn "-------------------------------"
    putStrLn "Calculate for weight capacity 20: " 
    putStrLn "-------------------------------"
    mapM_ print $ calculateItemsForCapacity 20 0 items
    --putStrLn "----------------------"
    --putStrLn "Calculate for Profit 5: "
    --putStrLn "----------------------"
    --mapM_ print $ calculateItemsForProfit 5 0 items
    
createItem :: String -> Item
createItem str = Item (read(head $ splitString str ',') ::Double) (read ( head $ drop 1 $ splitString str ',') ::Double) (head $ drop 2 $ splitString str ',') 0.0

splitString :: String -> Char -> [String]
splitString "" ch = []
splitString str ch = (partOne $ span(/= ch) str): (splitString (drop 1 $ partTwo $ span (/= ch) str) ch)

partOne (part,_) = part
partTwo (_,part) = part

calculateEfficiency :: [Item] -> [Item]
calculateEfficiency items = [ Item (weight i) (profit i) (item i) ((profit i)/(weight i)) | i <- items ]

qsort  ::  [Item] -> [Item]
qsort  [ ]  =  [ ] 
qsort (pivot:rest) = qsort [x | x <- rest, (efficiency x)>= (efficiency pivot)] ++ [pivot] ++   qsort [x | x <- rest, (efficiency x) < (efficiency pivot)] 

calculateItemsForCapacity :: Double -> Double-> [Item] -> [Item]
calculateItemsForCapacity target current (a:ax)
    | (current + (weight a)) <= target = a: calculateItemsForCapacity target (current+(weight a)) ax
    | otherwise = []
    
calculateItemsForProfit :: Double -> Double-> [Item] -> [Item]
calculateItemsForProfit target current (a:ax)
    | (current + (profit a)) <= target = a: calculateItemsForCapacity target (current+(profit a)) ax
    | otherwise = []
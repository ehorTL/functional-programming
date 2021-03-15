module Util where 

import DbObjects

softListToString :: [Software] -> String
softListToString lst = 
    if lst == [] then "Empty list. Nothing Found." else
        let 
            f (x:xs) c = show c ++ ". " ++ show x ++ "\n" ++ f xs (c+1)
            f _ _ = ""
                in
                    f lst 0

distrListToString :: [SoftDistribution] -> String
distrListToString lst = 
    if lst == [] then "Empty list. Nothing Found." else
        let 
            f (x:xs) c = show c ++ ". " ++ show x ++ "\n" ++ f xs (c+1)
            f _ _ = ""
                in
                    f lst 0

statRowToString :: [Statistics] -> String
statRowToString lst = if lst == [] then "Empty list. Nothing Found." else
        let 
            f (x:xs) c = show c ++ ". " ++ show x ++ "\n" ++ f xs (c+1)
            f _ _ = ""
                in
                    f lst 0

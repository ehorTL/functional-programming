module Util where 

import DbObjects

softListToString :: [Software] -> String
softListToString lst = let 
    f (x:xs) c = show c ++ ". " ++ show x ++ "\n" ++ f xs (c+1)
    f _ _ = ""
        in
            f lst 0
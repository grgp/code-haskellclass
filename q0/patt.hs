exOr :: Bool -> Bool -> Bool
exOr True True = False
exOr False True = True
exOr True False = True
exOr False False = False

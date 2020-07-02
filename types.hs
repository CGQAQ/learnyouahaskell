-- YesNo typeclass

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

{-
    *Main> yesno (0::Int)
    False
    *Main> yesno (1::Int)
    True
-}


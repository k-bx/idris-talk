import Data.Vect

total insSortKr : Ord elem => Vect n elem -> Vect n elem
insSortKr [] = []
insSortKr (x :: []) = [x]
insSortKr (x :: xs) = insertKr x (insSortKr xs)
  where
    -- insertKr : elem -> Vect n elem -> Vect (S n) elem
    insertKr el [] = [el]
    insertKr el (y :: ys) =
      case el > y of
        False => el :: y :: ys
        True => y :: (insertKr el ys)

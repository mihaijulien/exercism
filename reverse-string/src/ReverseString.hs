module ReverseString (reverseString) where


reverseString :: String -> String
reverseString [] = []
reverseString (x:xs) = reverseString xs ++ [x]

{--
Though elegant, this solution is seriously inefficient: it takes 𝒪 ( n 2 ) time to reverse a list of length n .

This happens because xs ++ ys takes 𝒪 ( len ( 𝚡𝚜 ) ) time. The longer xs, the longer it takes. The length of ys however doesn't matter.
--}

reverseString :: String -> String
reverseString str = last str : reverseString (init str)

{--

The functions head, tail, last, and init are ⚠️ partial ⚠️ and should basically never be used.

Instead of head and tail, use pattern matching:

-- Don't use `head` and `tail`...
_ = if null someList
  then d
  else f (head xs) (tail xs)

-- ...but pattern match instead.
_ = case someList of
  []     -> d
  x : xs -> f x xs

In addition to being dangerous, last and init are also inefficient. Both take 𝒪 ( n ) time on lists of length n . When you are tempted to use them, you are likely using the wrong data structure for the job.

If you really must use lists and you need both, then you are better off reverseing first and pattern matching on the result

--}

reverseString :: String -> String
reverseString str = foldl (flip (:)) [] str

{--

Recommendation: never use foldl; use foldl' instead.

foldl and foldl' both compute exactly the same result. The difference is in how they compute it. In short,

    foldl accumulates lazily, which in theory yields time savings in exchange for extra space usage, whereas
    foldl' accumulates eagerly, which in theory yields space savings in exchange for more time.

In practice however foldl tends to leak space, and foldl' is both more efficient and faster in the vast majority of cases. For this reason, always reach for foldl' first.

--}

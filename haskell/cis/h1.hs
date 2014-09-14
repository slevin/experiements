toDigits :: Integer -> [Integer]
toDigits inX = map (\x -> read [x]::Integer) (show inX)

toDigitsReverse :: Integer -> [Integer]
toDigitsReverse inX = reverse $ toDigits inX

-- doubles from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:rest) = x : y * 2 : doubleEveryOther rest
doubleEveryOther [x] = [x]
doubleEveryOther [] = []

sumDigits :: [Integer] -> Integer
sumDigits (x:xs)
  | (length $ toDigits x) == 1 = x + sumDigits xs
  | (length $ toDigits x) > 1  = (sumDigits $ toDigits x) + sumDigits xs
sumDigits [] = 0

validate :: Integer -> Bool
validate x = (sumDigits $ doubleEveryOther $ toDigitsReverse x) `mod` 10  == 0


-- hanoi methods
type Peg = String
type Move = (Peg, Peg)
type Stack = [Integer]
-- Game is
-- Stacks = current set of pegs with discs on them in order
-- Peg = Which peg is where to move the current stack
-- [Move] = the moves accumulator
type Game = (Stack, Stack, Stack, Peg, [Move])

makeHanoi :: Integer -> Peg -> Peg -> Peg -> Game
makeHanoi i a b c = ([1..i], [], [], "b", [])

playHanoi :: Stack -> Stack -> Stack -> Peg -> [Move] -> [Move]
playHanoi [] [] [] _ ms = ms
playHanoi s1 [] [] "b" ms = ms ++ (playHanoi (init s1) [] [] "c" ms) ++ [("a", "b")] ++ (playHanoi [] [] (init s1) "b" ms)
playHanoi s1 [] [] "c" ms = ms ++ (playHanoi (init s1) [] [] "b" ms) ++ [("a", "c")] ++ (playHanoi [] (init s1) [] "c" ms)
playHanoi [] s2 [] "a" ms = ms ++ (playHanoi [] (init s2) [] "c" ms) ++ [("b", "a")] ++ (playHanoi [] [] (init s2) "a" ms)
playHanoi [] s2 [] "c" ms = ms ++ (playHanoi [] (init s2) [] "a" ms) ++ [("b", "c")] ++ (playHanoi (init s2) [] [] "c" ms)
playHanoi [] [] s3 "a" ms = ms ++ (playHanoi [] [] (init s3) "b" ms) ++ [("c", "a")] ++ (playHanoi [] (init s3) [] "a" ms)
playHanoi [] [] s3 "b" ms = ms ++ (playHanoi [] [] (init s3) "a" ms) ++ [("c", "b")] ++ (playHanoi (init s3) [] [] "b" ms)

hanoi' :: Game -> [Move]
hanoi' (s1, s2, s3, t, ms) = playHanoi s1 s2 s3 t ms

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi i p1 p2 p3 = hanoi' (makeHanoi i p1 p2 p3)



{-
is there a way to abstract it
 its hardcoded with peg names
 and those things are basically the same



-}

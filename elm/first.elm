mytext = [markdown|

# Making Fancy Text

These are some text lines with a new paragraph

here starts a new one
but here is the same one

  1. *italic*
  2. **bold**
  3. `computery`
  4. [Links to things](http://www.google.com)

|]

dood = image 400 200 "http://upload.wikimedia.org/wikipedia/commons/thumb/d/da/Hermann_Hesse_2.jpg/501px-Hermann_Hesse_2.jpg"

{-
main = flow inward [ mytext
                 , dood
                 ]
-}

something = 5

fun : Int -> String
fun a = show a
       
another = fun "abc"

--main = collage 400 400 [ outlined (dashed green) (ngon 5 100) ]

box color = filled color (square 40)

main = collage 400 400 [ move (100, 100) (box red)
                       , scale 2 (box green)
                       , rotate (degrees 45) (box blue)
                       ]
                         
                              


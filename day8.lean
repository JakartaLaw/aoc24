def readFile (filePath : String) : IO String := do
let content <- IO.FS.readFile filePath
return content

def main : IO Unit := do
let content <- readFile "data/day8.txt"
IO.println s!"{content}"

structure Position where
  x : Int
  y : Int
deriving Repr

instance : Add Position where
  add p1 p2 := {x := p1.x + p2.x, y := p1.x + p2.x}

instance : Sub Position where
  sub p1 p2 := {x := p1.x - p2.x, y := p1.x - p2.x}

instance : Ord Position where
  compare p1 p2 :=
    match compare p1.y p2.x with
    | Ordering.eq => compare p1.x p2.x
    | ord => ord

def calcAntinodes (x y : Position) : (Position × Position) :=
  let A := x + (x - y)
  let B := y + (y - x)
  (A, B)




#eval calcAntinodes {x := 1, y := 1} {x := 3, y := 3}

#eval main

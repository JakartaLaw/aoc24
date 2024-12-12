def readFile (filePath : String) : IO String := do
let content <- IO.FS.readFile filePath
return content

structure Position where
  x : Nat
  y : Nat
deriving Repr

structure Boundaries where
  xmin : Nat
  xmax : Nat
  ymin : Nat
  ymax : Nat
deriving Repr

#eval [Position.mk 1 2, Position.mk 2 9]

structure Spot where
  height : Nat
  bnds : Boundaries
  pos : Position
deriving Repr

def charToSpot (c: Char) (bnds: Boundaries) (x y : Nat) : Spot :=
  match c.toString.toNat? with
  | some n => Spot.mk n bnds (Position.mk x y)
  | _ => Spot.mk 99 bnds (Position.mk x y)

def parseFile (content : String) : List (List Spot) :=
  let lines := content.splitOn "
  "
  -- clunky
  let ymax := lines.length
  let xmax :=
    match lines.get? 0 with
    | some n => n.length
    | _ => 99999

  let bnds := Boundaries.mk 0 xmax 0 ymax

  lines |>.enum.map (
    fun (idy, l) => l.toList.enum.map (
      fun (idx, height) => charToSpot height bnds idx idy
    )
  )

-- best way to solve: filter all by 0, (starts) generate a new list o

def main : IO Unit := do
  let content <- readFile "data/day10.txt"
  let topMap := parseFile content
  --IO.println s!"{topMap.get 0 |>.height }"

#eval main

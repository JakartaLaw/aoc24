def readFile (filePath : String) : IO String := do
let content <- IO.FS.readFile filePath
return content




structure Position where
  x : Int
  y : Int
deriving Repr

def examplePosition : Position :=
  { x := 1, y := 2}

-- #eval examplePosition

structure Antenna where
  name : Char
  pos : Position
deriving Repr

def exampleAntenna : Antenna :=
  {name := 'C' , pos := {x := 1, y := 2}}

-- #eval exampleAntenna

def parseLine (line : String) (rowIx : Nat) : List Antenna :=
  line.toList.enum
   |>.map (fun (colIx, c) => {name := c, pos := {x := colIx, y:=rowIx}})
   |>.filter (fun a => a.name != '.')


#eval parseLine ".....aX.X.." 2

def parseGrid (content : String) : List Antenna :=
  let lines : List String := content.splitOn "
"
  lines.enum
  |>.flatMap (fun (rowIx, line) => parseLine line rowIx)

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

def getUniqueAntennaNames (antennas : List Antenna) : List Char :=
  antennas
    |>.map (fun a => a.name)
    |>.foldl (fun acc x =>
      match acc.contains x with
      | true => acc          -- If `x` is already in the list, skip it
      | false => acc ++ [x]  -- If not, append `x`
    ) []

def getAllAntinodes (antennaNames: List Char) (antennas : List Antenna) : List Position :=
  antennaNames
  |>.map


def main : IO Unit := do
let content <- readFile "data/day8.txt"
let antennaGrid := parseGrid content
let uniqueAntennaNames := getUniqueAntennaNames antennaGrid


IO.println s!"{uniqueAntennaNames}"


#eval calcAntinodes {x := 1, y := 1} {x := 3, y := 3}

#eval main

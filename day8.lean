def readFile (filePath : String) : IO String := do
let content <- IO.FS.readFile filePath
return content

structure Position where
  x : Int
  y : Int
deriving Repr

instance : ToString Position where
  toString p := s!"({p.x}, {p.y})"

instance : Add Position where
  add p1 p2 := {x := p1.x + p2.x, y := p1.y + p2.y}

instance : Sub Position where
  sub p1 p2 := {x := p1.x - p2.x, y := p1.y - p2.y}

instance : Ord Position where
  compare p1 p2 :=
    match compare p1.y p2.x with
    | Ordering.eq => compare p1.x p2.x
    | ord => ord

instance : BEq Position where
  beq p1 p2 := p1.x == p2.x && p1.y == p2.y

def examplePosition : Position :=
  { x := 1, y := 2}

-- #eval examplePosition

structure Antenna where
  name : Char
  pos : Position
deriving Repr

instance : ToString Antenna where
  toString a := s!"({a.name}, {a.pos})"

--def exampleAntenna : Antenna :=
--  {name := 'C' , pos := {x := 1, y := 2}}

-- #eval exampleAntenna

structure AntennaPair where
  a1 : Antenna
  a2 : Antenna
deriving Repr

instance : ToString AntennaPair where
  toString ap := s!"a1: {ap.a1.name}, a2: {ap.a2.name}"

instance : BEq AntennaPair where
  beq ap1 ap2 :=
    if ap1.a1.pos == ap2.a1.pos && ap1.a2.pos == ap2.a2.pos then true -- SAME ORDER OF PAIRS
    else if ap1.a1.pos == ap2.a2.pos && ap1.a2.pos == ap2.a1.pos then true -- DIFF ORDER OF PAIRS
    else false

-- def testEquivalenceAntennaPair :=
--   let ap1 := AntennaPair.mk (Antenna.mk 'c' (Position.mk 2 1)) (Antenna.mk 'c' (Position.mk 5 6))
--   let ap2 := AntennaPair.mk (Antenna.mk 'c' (Position.mk 5 6)) (Antenna.mk 'c' (Position.mk 2 1))
--   ap1 == ap2

-- #eval testEquivalenceAntennaPair

def parseLine (line : String) (rowIx : Nat) : List Antenna :=
  line.toList.enum
   |>.map (fun (colIx, c) => {name := c, pos := {x := colIx, y:=rowIx}})
   |>.filter (fun a => a.name != '.')


#eval parseLine ".....aX.X.." 2

def parseGrid (content : String) : List Antenna :=
  let lines : List String := content.splitOn "
"
  lines.enum
  |>.bind (fun (rowIx, line) => parseLine line rowIx)


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

-- diff name
def getAntennasByName (antennaName: Char) (antennas : List Antenna) : List Antenna :=
  antennas
    |>.filter (fun a => a.name == antennaName)

def getAntennaPairs (antennas : List Antenna) (antennaPairs : List AntennaPair) : List AntennaPair :=
  match antennas with
  | [] => antennaPairs
  | a :: tail =>
    let newAntennaPairs := antennaPairs ++ (tail.map (fun antenna => AntennaPair.mk a antenna))
    getAntennaPairs tail newAntennaPairs

-- def testGetAntennaPairs : List AntennaPair :=
--   getAntennaPairs [(Antenna.mk 'c' (Position.mk 1 2)), (Antenna.mk 'c' (Position.mk 1 9)), (Antenna.mk 'c' (Position.mk 6 6))] []
-- #eval testGetAntennaPairs

def createAntinodes (antennaPairs : List AntennaPair) : List Position :=
  antennaPairs
    |>.foldl (
      fun acc a =>
      let (a1, a2) := calcAntinodes a.a1.pos a.2.pos
      acc ++ [a1, a2]
    ) []


def testCreateAntinodes : List Position :=
  createAntinodes (getAntennaPairs [(Antenna.mk 'c' (Position.mk 1 2)), (Antenna.mk 'c' (Position.mk 1 10)), (Antenna.mk 'c' (Position.mk 6 6))] [])

#eval testCreateAntinodes

def antinodesPipeline (antennaName : Char) (antennas : List Antenna) : List Position :=
  antennas
    |> getAntennasByName antennaName
    |> (fun filtered => getAntennaPairs filtered [])
    |> createAntinodes

def createAllAntinodes (antennas: List Antenna) (antennaNames: List Char) : List Position :=
  antennaNames --actually is antennas
    |>.bind (fun antennaName => antinodesPipeline antennaName antennas)

def uniquePositions (positions : List Position) : List Position :=
  positions.foldl (fun acc pos =>
    if acc.contains pos then acc else acc ++ [pos]
  ) []

def withinBoundsPositions (position : Position ) (xmin xmax ymin ymax : Nat) : Bool :=
  if position.x >= xmax then false
  else if position.x < xmin then false
  else if position.y >= ymax then false
  else if position.y < ymin then false
  else true

def getGridBounds (content: String) : (Nat × Nat × Nat × Nat) :=
  let lines : List String := content.splitOn "
"
  let ymax := lines.length
  let xmax :=
    match lines.get? 0 with
    | some line => line.toList.length
    | _ => 0
  (0, xmax, 0, ymax)

def main : IO Unit := do
  let content <- readFile "data/day8.txt"
  let (xmin, xmax, ymin, ymax) := getGridBounds content
  let antennaGrid := parseGrid content
  IO.println s!"{(xmin, xmax, ymin, ymax)}"
  let uniqueAntennaNames := getUniqueAntennaNames antennaGrid
  let antinodes := createAllAntinodes antennaGrid uniqueAntennaNames
  let correctAntinodes :=
    antinodes
      |> uniquePositions
      |>.filter (fun position => withinBoundsPositions position xmin xmax ymin ymax)

  IO.println s!"{correctAntinodes.length}"

def equivPosition : Bool :=
  let p1 := Position.mk 2 1
  let p2 := Position.mk 2 1
  p1 == p2

#eval equivPosition

#eval calcAntinodes {x := 1, y := 2} {x := 3, y := 1}

#eval main

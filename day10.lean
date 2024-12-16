def readFile (filePath : String) : IO String := do
let content <- IO.FS.readFile filePath
return content

structure Position where
  x : Nat
  y : Nat
deriving Repr

instance : ToString Position where
  toString p := s!"Position(x: {p.x}, y: {p.y})"

instance : BEq Position where
  beq p1 p2 := p1.x == p2.x && p1.y == p2.y

structure Boundaries where
  xmin : Nat
  xmax : Nat
  ymin : Nat
  ymax : Nat
deriving Repr

instance : ToString Boundaries where
  toString b := s!"Boundaries(xmin: {b.xmin}, xmax: {b.xmax}, ymin: {b.ymin}, ymax: {b.ymax})"

instance : ToString Boundaries where
  toString b := s!"Boundaries(xmin: {b.xmin}, xmax: {b.xmax}, ymin: {b.ymin}, ymax: {b.ymax})"

structure Neighbourhood where
  up : Option Position
  down : Option Position
  left : Option Position
  right : Option Position
deriving Repr

instance : ToString Neighbourhood where
  toString n :=
    let upStr := match n.up with
      | some p => toString p
      | none   => "none"
    let downStr := match n.down with
      | some p => toString p
      | none   => "none"
    let leftStr := match n.left with
      | some p => toString p
      | none   => "none"
    let rightStr := match n.right with
      | some p => toString p
      | none   => "none"
    s!"Neighbourhood(up: {upStr}, down: {downStr}, left: {leftStr}, right: {rightStr})"

-- #eval [Position.mk 1 2, Position.mk 2 9]

structure Spot where
  height : Nat
  bnds : Boundaries
  pos : Position
deriving Repr

instance : ToString Spot where
  toString s := s!"Spot(height: {s.height}, bnds: {toString s.bnds}, pos: {toString s.pos})"

inductive Direction where
  | Up
  | Down
  | Left
  | Right
deriving Repr

instance : ToString Direction where
  toString d := match d with
    | Direction.Up    => "Up"
    | Direction.Down  => "Down"
    | Direction.Left  => "Left"
    | Direction.Right => "Right"

def charToSpot (c: Char) (bnds: Boundaries) (x y : Nat) : Spot :=
  match c.toString.toNat? with
  | some n => Spot.mk n bnds (Position.mk x y)
  | _ => Spot.mk 99 bnds (Position.mk x y)

def legalNeighbour (pos : Position) (bnds : Boundaries) : Option Position :=
  -- potential off by one error
  if pos.x >= bnds.xmin && pos.x < bnds.xmax && pos.y >= bnds.ymin && pos.y < bnds.ymax
  then pos
  else none

def withinRange (value : Int) (minVal maxVal : Int) : Bool :=
  value >= minVal && value < maxVal

def tryCalcNat (x y : Int) (bnds : Boundaries) : Bool :=
  (withinRange x bnds.xmin bnds.xmax) &&
  (withinRange y bnds.ymin bnds.ymax)

def tryCreatePosition (x y : Int) (bnds : Boundaries) : Option Position :=
  if tryCalcNat x y bnds then
    Position.mk x.toNat y.toNat
  else
    none

def calcNewPosition (pos : Position) (dir : Direction) (bnds : Boundaries) : Option Position :=
  match dir with
  | Direction.Up => tryCreatePosition pos.x (pos.y - 1) bnds
  | Direction.Down => tryCreatePosition pos.x (pos.y + 1) bnds
  | Direction.Left => tryCreatePosition (pos.x - 1) pos.y bnds
  | Direction.Right => tryCreatePosition (pos.x + 1) pos.y bnds

def getNeighbourhood (s : Spot) : Neighbourhood :=
  (Neighbourhood.mk
   (calcNewPosition s.pos Direction.Up s.bnds)
   (calcNewPosition s.pos Direction.Down s.bnds)
   (calcNewPosition s.pos Direction.Left s.bnds)
   (calcNewPosition s.pos Direction.Right s.bnds)
  )

def findSpot (topMap : List (List Spot)) (x : Position) : Option Spot :=
  topMap.join
    |>.filter (fun s => s.pos == x)
    |>.head?

def getNeighbourhoodSpots (nbh : Neighbourhood) (topMap : List (List Spot)): List Spot :=
  [nbh.up, nbh.down, nbh.left, nbh.right]
    |>.filterMap id
    |>.map ( fun pos =>
      findSpot topMap pos
    )
    |>.filterMap id

def updateTrail (trail: List Spot ) (topMap: List (List Spot)) : List (List Spot) :=

  let currentSpot := trail.head?
  match currentSpot with
  | some cs =>
    let nbh := getNeighbourhood cs |> Option.map (fun n => getNeighbourhoodSpots n topMap) |>.getD []

    nbh.foldl (fun acc spot =>
      if spot.height - 1 == cs.height then
        acc ++ [[spot] ++ trail]
      else
        acc
    ) []
  | _ => [[]] -- should never get to here

#eval getNeighbourhood (Spot.mk 0 (Boundaries.mk 0 10 0 10) (Position.mk 2 0) )

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
  let nbhSpots := getNeighbourhoodSpots (getNeighbourhood (Spot.mk 0 (Boundaries.mk 0 45 0 45) (Position.mk 4 0))) topMap
  let initSpots := topMap.join
    |>.filter (fun x => x.height == 0)
    |>.map (fun x => [x])

  let h1 :=
  initSpots.foldl (fun acc trail =>
    acc ++ [updateTrail trail topMap].join
  ) []
  let h2 :=
  h1.foldl (fun acc trail =>
    acc ++ [updateTrail trail topMap].join
  ) []
  let h3 :=
  h2.foldl (fun acc trail =>
    acc ++ [updateTrail trail topMap].join
  ) []
  let h4 :=
  h3.foldl (fun acc trail =>
    acc ++ [updateTrail trail topMap].join
  ) []

  IO.println s!"{h4.length}"

  --IO.println s!"{topMap.get 0 |>.height }"

#eval main

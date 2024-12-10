def readFile (filePath : String) : IO String := do
let content <- IO.FS.readFile filePath
return content

-- ".", "#", "^", "B", "V"

inductive Tile where
| notVisited
| visited
| currPos
| boundaryTile
| obstackle
| unknown
deriving Repr

instance : BEq Tile where
  beq t1 t2 :=
    match t1, t2 with
    | Tile.notVisited, Tile.notVisited => true
    | Tile.visited, Tile.visited       => true
    | Tile.currPos, Tile.currPos       => true
    | Tile.boundaryTile, Tile.boundaryTile => true
    | Tile.obstackle, Tile.obstackle   => true
    | Tile.unknown, Tile.unknown       => true
    | _, _ => false

def charToTile (s: Char) : Tile :=
match s with
| '.' => Tile.notVisited
| '^' => Tile.currPos
| '#' => Tile.obstackle
| _   => Tile.unknown -- Return none for invalid characters

def tokenizeContent (s: String) : List (List Tile) :=
let lines := s.splitOn "
"
lines |>.map (
  fun l => l.toList.map charToTile
)

def tileToChar (t : Tile) : String :=
match t with
| Tile.notVisited => "."
| Tile.boundaryTile => "B"
| Tile.obstackle => "X"
| Tile.currPos => "^"
| Tile.visited => "o"
| _ => "LARS"

def reprTokens (ll : List (List Tile)) : List (List String) :=
ll.map (
  fun l => l.map tileToChar
)

inductive Direction
| up
| left
| right
| down
deriving Repr, BEq

structure GameState where
  x : Int
  y : Int
  direction : Direction
  tiles : List (List Tile)
deriving Repr

def getInitialPos (ls : List (List Tile)) : Option GameState :=
  ls.enum.foldl (fun acc (rowIdx, row) =>
    match acc with
    | some pos => some pos -- If already found, skip further processing
    | none =>
      match row.enum.find? (fun (_, tile) => tile == Tile.currPos) with
      | some (colIdx, _) => some { x := rowIdx, y := colIdx, direction := Direction.up, tiles := ls}  -- Found current position
      | none => none -- Not found in this row
  ) none

def nextDir (dir : Direction) : Direction :=
match dir with
| Direction.up => Direction.right
| Direction.right => Direction.down
| Direction.down => Direction.left
| Direction.left => Direction.up

def changeDir (gs: GameState) : GameState :=
  {gs with direction := nextDir gs.direction}

--def getToken (x : Int) (y: Int ) (gs: GameState) : Option Tile :=
--  gs.tiles.get? x.toNat >>= fun row => row.get? x.toNat

def getToken (x : Nat) (y : Nat) (gs : GameState) : Tile :=
  gs.tiles.getD y [] |>.getD x Tile.notVisited

def getNextXY (gs: GameState) : GameState :=
  let dir := gs.direction
  let y := gs.y
  let x := gs.x
  match dir with
  | Direction.up => { gs with y := y - 1 }
  | Direction.down => { gs with y := y + 1 }
  | Direction.left => { gs with y := x - 1 }
  | Direction.right => { gs with y := x + 1 }

def evaluateDirection (gs: GameState) (n_directions: Nat): GameState :=
  if h01 : n_directions == 0 then gs -- Stop if maximum direction changes are reached
  else
    let gs1 := getNextXY gs
    let tile := getToken gs.x.toNat gs.y.toNat gs1
    match tile with
    | Tile.obstackle =>
      have h0: n_directions - 1 < n_directions := by
        apply Nat.sub_one_lt
        simp [*] at h01
        assumption
      evaluateDirection (changeDir gs) (n_directions - 1)
    | _ => gs1
--termination_by n_directions

#check evaluateDirection


def move (gs: GameState) : IO Unit :=
  let tiles := gs.tiles
  tiles.enum.map (fun (rowIdx, row) =>
    row.enum.map (fun (colIdx, tile) =>
      |
    )
  )

#check ["a", "b"].enum.find?


-- Example usage
#eval getInitialPos [[Tile.notVisited, Tile.notVisited, Tile.notVisited], [Tile.notVisited, Tile.currPos, Tile.notVisited], [Tile.notVisited, Tile.notVisited, Tile.notVisited]]


#eval reprTokens (tokenizeContent ".#.
.^.
..#")

def genBoundaryTokens (ls: List (List Tile)) : List (List Tile) :=
  let bottomRowId := ls.length - 1 -- Last row index
  let rightMostRowLength := ls.get? 0 |>.map (fun innerList => innerList.length) |>.getD 0 -- Width of the first row
  ls.enum.map (fun (rowIdx, row) =>
    row.enum.map (fun (colIdx, tile) =>
      if (rowIdx == 0 || rowIdx == bottomRowId || colIdx == 0 || colIdx == rightMostRowLength - 1) && tile == Tile.notVisited then
        Tile.boundaryTile
      else
        tile
    )
  )


#eval genBoundaryTokens [[Tile.obstackle, Tile.notVisited, Tile.notVisited], [Tile.notVisited, Tile.currPos, Tile.notVisited], [Tile.notVisited, Tile.notVisited, Tile.notVisited]]



def main : IO Unit := do
let content <- readFile "data/day6.txt"
let tokens := tokenizeContent content
let tokens := genBoundaryTokens tokens
let strTokens := reprTokens tokens
IO.println s!"{strTokens}"

--#eval main

--#check List

def readFile (filePath : String) : IO String := do
let content <- IO.FS.readFile filePath
return content

def integize (s : String) : Int :=
match s.toInt? with
| some n => n
| _ => panic! s!"Not integer {s}"

def parseFile(content : String) : List (List Int) :=
  let lines : List String := content.splitOn "
"
lines |>.map (fun line =>
  line.splitOn " "
    |>.map integize
)

def checkSafety (x y : Int) : Bool :=
  y > x && (y - x).natAbs < 4

def isIncreasing : List Int -> Bool
| [] => true -- empty list
| [_] => true -- 1 item list
| x :: y :: rest => if (checkSafety x y) then isIncreasing (y :: rest) else false

def isDecreasing : List Int -> Bool
| [] => true
| [_] => true
| x :: y :: rest => if (checkSafety y x) then isDecreasing (y :: rest) else false

def isIncreasingSafe (l : List Int) : Bool :=
let n := l.zip l.tail |>.foldl
  (fun acc (prev, curr) =>
    if (checkSafety prev curr) then acc + 1 else acc
  ) 0
if n > 1 then false else true

def isDecreasingSafe (l : List Int) : Bool :=
let n := l.zip l.tail |>.foldl
  (fun acc (prev, curr) =>
    if (checkSafety curr prev) then acc + 1 else acc
  ) 0
if n > 1 then false else true

def isIncOrDec (l : List Int) : Bool :=
let bDec := isDecreasing l
let bInc := isIncreasing l
if bDec || bInc then true else false

def isIncOrDec2 (l : List Int) : Bool :=
let bDec := isDecreasingSafe l
let bInc := isIncreasingSafe l
if bDec || bInc then true else false

def filterUnsafe1 (l: List (List Int)) : List (List Int) :=
  l.filter isIncOrDec

def filterUnsafe2 (l: List (List Int)) : List (List Int) :=
  l.filter isIncOrDec2

def getCount (l: List (List Int)) : Int :=
  l.length

def main : IO Unit := do
let content <- readFile "data/day2.txt"
let levels := parseFile content
let filtered := filterUnsafe1 levels
let nfiltered := getCount filtered
IO.println s!"number of safe {nfiltered}"
let filtered2 := filterUnsafe2 levels
let nfiltered2 := getCount filtered2
IO.println s!"number of safe {filtered2}"

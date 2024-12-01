def readFile (filePath : String) : IO String := do
let content <- IO.FS.readFile filePath
return content

def integize (s : String) : Int :=
match s.toInt? with
| some n => n
| none => 0

def parseFileContent (content: String) : List (Int × Int) :=
let lines : List String := content.splitOn "
" -- does not split correctly currently
lines |>.map (fun line =>
 match line.splitOn "   " with
 | [first, second] => (integize first, integize second)
 | _ => panic! s!"Malformed line: '{line}'"
)

def sortList (l : List Int) : List Int :=
  let arr := l.toArray
  let sortedArr := arr.qsort (· < ·)
  sortedArr.toList

def transposeData (data: List (Int × Int)) : List (List Int) :=
let (list1, list2) := data.unzip
[sortList list1, sortList list2]

def calcDifferences (ls : List (List Int)) : List Int :=
match ls with
| [list1, list2] =>
list1.zip list2 |>.map (fun (a, b) => (a - b).natAbs)
| _ => panic! "Expected two lists"

def sumDifferences (l : List Int) : Int :=
l.foldl ( fun acc x => acc + x) 0

def main : IO Unit := do
let content <- readFile "data/day1.txt"
let data := transposeData (parseFileContent content)
let differences := calcDifferences data
let total_diff := sumDifferences differences
IO.println s!"File content ¬ {total_diff}"

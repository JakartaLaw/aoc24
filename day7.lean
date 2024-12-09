def readFile (filePath : String) : IO String := do
let content <- IO.FS.readFile filePath
return content

def integize (s : String) : Int :=
match s.toInt? with
| some n => n
| _ => panic! s!"Not integer {s}"

def parseLine (line : String) : Int × List Int :=
  let splitOne := line.splitOn ":"
  let l := splitOne.tail
    |>.bind (fun x => x.splitOn " ")
    |>.filter (fun s => s != "")
    |>.map integize
  let result := match splitOne.get? 0 with
    | some v => integize v
    | _ => 99999
  (result, l)

#eval parseLine "432832280199: 3 286 4 3 17 682 7 7 9 2"

def parseFile(content : String) : List (Int × List Int) :=
  let lines : List String := content.splitOn "
"
lines |>.map parseLine


def calcTestValue (result: Int) (l : List Int) : Bool :=
  let additionSum := l |>.foldl (fun acc x => acc + x) 0
  let multiplicationSum := l |>.foldl (fun acc x => acc * x) 1
  additionSum == result || multiplicationSum == result

--#eval calcTestValue 120 [11, 10]

inductive Operation
| Add
| Multiply
| Concat
deriving Repr

def concatIntegize (x y : Int) : Int :=
  let concattedStrings := toString x ++ toString y
  integize concattedStrings

-- #eval concatIntegize 2 14

def ApplyOperation (op : Operation) (x y : Int) : Int :=
  match op with
  | Operation.Add => x + y
  | Operation.Multiply => x * y
  | Operation.Concat => concatIntegize x y

def extendCombinations (acc : List (List Operation)) (ops : List Operation) : List (List Operation) :=
  acc.bind (fun l => ops.map (fun op => l ++ [op]))

-- #eval extendCombinations [[Operation.Add, Operation.Multiply]] [Operation.Add, Operation.Multiply]

def genListOfOperations (n : Nat) : List (List Operation) :=
  let ops := [Operation.Add, Operation.Multiply, Operation.Concat]
  let placeholders : List Unit := List.replicate n ()
  List.foldl (fun acc _ => extendCombinations acc ops) [[]] placeholders

-- #eval genListOfOperations 5

def applyOperationList (l : List Int) (ops : List Operation) : Int :=
  let v0 :=
  match l.get? 0 with
  | some v => v
  | _ => 0
  l.tail.zip ops |>.foldl (fun acc (v, op) => ApplyOperation op acc v) v0

-- #eval applyOperationList [1, 2, 3] [Operation.Add, Operation.Multiply]

def correctResult (result : Int) (v : Int) : Nat :=
  if result == v then 1 else 0

def checkList (result: Int) (l : List Int) : Int :=
  let opsCombs := genListOfOperations (l.length - 1)
  let allResults := opsCombs.map (fun ops => applyOperationList l ops)
  let correctCounter := allResults.foldl (fun acc v => acc + (correctResult result v)) 0
  if correctCounter > 0 then result else 0

-- #eval checkList 12 [1, 2]

def checkAll (rl : List (Int × List Int)) : Int :=
  let correctList := rl.map (fun (result, l) => checkList result l)
  correctList |>.foldl (fun acc x => acc + x) 0

def main : IO Unit := do
let content <- readFile "data/day7.txt"
let rls := parseFile content
let correctCounter := checkAll rls
IO.println s!"{correctCounter}"

#eval main

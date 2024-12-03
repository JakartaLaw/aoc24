import Regex

def readFile (filePath : String) : IO String := do
let content <- IO.FS.readFile filePath
return content

def splitByString(s: String) : List String :=
s.splitOn "mul" |>.tail!.map (fun part => "mul" ++ part)

def parseExpression (s : String) := do
  let re := regex% r"mul\\((\\d+),(\\d+)\\)" -- Regex for matching "mul(<INT>,<INT>)"
  -- only return String if captures has one.
  let captures := re.captures s
  IO.println s!"captures: {captures}"

def parseAllExpressions2 (l : List String) : IO Unit := do
  let re := regex% r"mul\((\d+),(\d+)\)"
  l.mapM (fun s => do
    let captures := re.captures s
    IO.println s!"captures: {captures}"
  )
  return ()

def main : IO Unit := do
let content <- readFile "data/day3.txt"
let muls := splitByString content
parseAllExpressions2 muls

-- match muls.head? with
-- | some firstMul => parseExpression firstMul
-- | none => IO.println "No MUL"

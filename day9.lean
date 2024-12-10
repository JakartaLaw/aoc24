def readFile (filePath : String) : IO String := do
let content <- IO.FS.readFile filePath
return content

--computer file
structure CompFile where
  id : Nat
  size : Nat
  pos : Nat
deriving Repr

instance : ToString CompFile where
  toString f := s!"id: {f.id} size: {f.size}, pos: {f.pos}"

structure FreeSpace where
  size : Nat
  pos : Nat
deriving Repr

instance : ToString FreeSpace where
  toString fs := s!"size: {fs.size}, pos: {fs.pos}"

def integizeChar (c : Char) : Option Nat :=
  c.toString.toNat?

--def parseFile (content : String) : ((List CompFile) × (List FreeSpace)) :=
def parseFile (content : String) : List CompFile :=
  let files := content.toList.enum
    |>.filter (fun (idx, size) => idx % 2 == 0)
    |>.map (fun (_, size) =>
      match integizeChar size with
      | some n => n
      | _ => panic! s!"Wrong character {size}"
    )
    |>.enum.map (fun (idx, size) => CompFile.mk idx size 1)

  let freeSpace := content.toList.enum
    |>.filter (fun (idx, size) => idx % 2 != 0)
    |>.map (fun (_, size) =>
      match integizeChar size with
      | some n => n
      | _ => panic! s!"Wrong character {size}"
    )
    |>.map (fun FreeSpace.mk size 1)
    (files, freeSpace)

def main : IO Unit := do
  let content <- readFile "data/day9.txt"
  let parsed := parseFile content
  IO.println s!"{parsed}"


#eval main


#eval integizeChar '2'

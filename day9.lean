def readFile (filePath : String) : IO String := do
let content <- IO.FS.readFile filePath
return content

--computer file (Thanks Jonas)
inductive MemoryType where
| File
| Space
deriving Repr

instance : BEq MemoryType where
  beq mt1 mt2 :=
    match mt1, mt2 with
    | MemoryType.File, MemoryType.File => true
    | MemoryType.Space, MemoryType.Space => true
    | _, _ => false

instance : ToString MemoryType where
  toString mt :=
    match mt with
    | MemoryType.File => s!"File"
    | MemoryType.Space => s!"Space"

structure CompFile where
  id : Nat
  size : Nat
  pos : List Nat
deriving Repr

instance : ToString CompFile where
  toString f := s!"id: {f.id} size: {f.size}, pos: {f.pos}"

structure FreeSpace where
  size : Nat
  pos : List Nat
deriving Repr

instance : ToString FreeSpace where
  toString fs := s!"size: {fs.size}, pos: {fs.pos}"

def integizeChar (c : Char) : Nat :=
  match c.toString.toNat? with -- risky business
  | some n => n
  | _ => 0
  --| _ => panic! s!"This Was Not a Real Number {c}"

def parseInfo (content : String) : List (MemoryType × Nat × Nat × Nat) :=
  content.toList.enum
    |>.map (
      fun (idx, size) =>
        if idx % 2 == 0 then (MemoryType.File, integizeChar size, idx / 2)
        else (MemoryType.Space, integizeChar size, idx)
    )
    |>.foldl (
      fun acc (memtype, size, idx) =>
      let position :=
        match acc.get? 0 with
        | some (_memtype, _size, _idx, _position) => _position + _size
        | _ => 0
      [(memtype, size, idx, position)] ++ acc
    ) []
    |> List.reverse

def getCompfiles (l : List (MemoryType × Nat × Nat × Nat)) : List CompFile :=
  l
  |>.filter (fun (memtype, _, _, _) => memtype == MemoryType.File)
  |>.map (fun (_, size, idx, position) => CompFile.mk idx size position)

def getFreeSpace (l : List (MemoryType × Nat × Nat × Nat)) : List FreeSpace :=
  l
  |>.filter (fun (memtype, _, _, _) => memtype == MemoryType.Space)
  |>.map (fun (_, size, _, position) => FreeSpace.mk size position)


def main : IO Unit := do
  let content <- readFile "data/day9.txt"
  let parsed := parseInfo content
  let compfiles := getCompfiles parsed
  let freespaces := getFreeSpace parsed
  IO.println s!"{compfiles}"
  IO.println s!"{freespaces}"


#eval main


--#eval integizeChar '2'

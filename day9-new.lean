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
  pos : Nat
deriving Repr

instance : ToString CompFile where
  toString f := s!"id: {f.id}, pos: {f.pos}"

instance : Ord CompFile where
  compare f1 f2 := compare f1.pos f2.pos

structure FreeSpace where
  pos : Nat
deriving Repr

instance : ToString FreeSpace where
  toString f := s!"pos: {f.pos}"

inductive MemoryBlock where
| File (f : CompFile)
| Space (s : FreeSpace)
deriving Repr

instance : ToString MemoryBlock where
  toString mb :=
    match mb with
    | MemoryBlock.File f => s!"File: {f}"
    | MemoryBlock.Space s => s!"Space: {s}"

def integizeChar (c : Char) : Nat :=
  match c.toString.toNat? with -- risky business
  | some n => n
  | _ => 0

def createObs (ftype : MemoryType) (size : Nat) (n : Nat) (idx: Nat) : ((List MemoryBlock) × Nat) :=
  let id := idx / 2 -- remembering that id is half of idx
  let memoryBlocks := List.range size |>.map ( fun x =>
  let ctr := x + n
    match ftype with
    | MemoryType.File => MemoryBlock.File (CompFile.mk id ctr)
    | MemoryType.Space => MemoryBlock.Space (FreeSpace.mk ctr)
  )
  (memoryBlocks, n + size)

--#eval createObs MemoryType.File 2 0 12

def parseFile (content : String) : List MemoryBlock :=
    let (blocks, _) := content.toList.enum
    |>.foldl (
      fun (acc, n) (idx, size) =>
      let memType := if idx % 2 == 0 then MemoryType.File else MemoryType.Space
      let (meml, nUpdated) := createObs memType (integizeChar size) n idx
      (acc ++ meml, nUpdated)
    ) ([], 0)
    blocks

def swapMemory (cf: CompFile) (fs: Option FreeSpace) : CompFile :=
  match fs with
  | some fs =>
    if cf.pos > fs.pos then
      CompFile.mk cf.id fs.pos
    else
      cf
  | none => cf

-- def zipWithOption {α β} (xs : List α) (ys : List β) : List (α × Option β) :=
--   match xs, ys with
--   | [], _ => []
--   | x :: xs, [] =>
--     (x, none) :: zipWithOption xs [] -- Recursive call reducing `xs`
--   | x :: xs, y :: ys =>
--     (x, some y) :: zipWithOption xs ys -- Recursive call reducing both `xs` and `ys`

def zipWithOptionAlt (cfs : List CompFile) (fss : List FreeSpace) : List (CompFile × Option FreeSpace) :=
  let optionFss := fss.map some -- Convert `List FreeSpace` to `List (Option FreeSpace)`
  let appendLength := (cfs.length - fss.length)
  let placeholders := List.replicate appendLength ()
  let l := placeholders |>.map (fun _ => none )
  if fss.length <= cfs.length then
    let extendedOptionFss := optionFss ++ l
    cfs.zip extendedOptionFss
  else
    cfs.zip optionFss

def compress (parsed : List MemoryBlock) : List CompFile :=
  let compfiles := parsed
    |>.filterMap (fun x =>
      match x with
      | MemoryBlock.File cf => some cf -- Extract `CompFile` if it's a `File`
      | MemoryBlock.Space _ => none   -- Ignore `FreeSpace`
    )
    |>.reverse
  --compfiles
  let freespaces := parsed
    |>.filterMap (fun x =>
      match x with
      | MemoryBlock.Space fs => some fs -- Extract `CompFile` if it's a `File`
      | MemoryBlock.File _ => none   -- Ignore `FreeSpace`
    )
  zipWithOptionAlt compfiles freespaces
  |>.map (fun (cf, fs) => swapMemory cf fs)

-- { gs with y := y - 1 }
def getTotalNumber (compfiles : List CompFile) : Nat :=
  compfiles
    |>.foldl (fun acc x =>
      acc + (x.pos * x.id)
    ) 0


-- #eval List.replicate 4 ()

def main : IO Unit := do
  let content <- readFile "data/day9.txt"
  let parsed := parseFile content
  let compfiles := compress parsed
  let totalnumber := getTotalNumber compfiles

  IO.println s!"{totalnumber}"

#eval main


--#eval integizeChar '2'

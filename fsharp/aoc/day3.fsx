open System.IO

type Square =
    | Tree
    | OpenSpace

let lines =
    File.ReadAllLines "./fsharp/aoc/input_3.txt"

let lineToRow line =
    Array.map (fun c ->
        match c with
        | '#' -> Tree
        | _ -> OpenSpace) (Seq.toArray line)

type Forest = { Map: Square [] [] }

let forestFromText lines = { Map = Array.map lineToRow lines }

let squareAt forest (x, y) =
    let height = forest.Map.Length
    let width = forest.Map.[0].Length
    if y >= height then None else Some forest.Map.[y].[x % width]

let countTrees forest (x, y) (dx, dy) =
    let rec _move acc forest (x, y) =
        let thingAt = squareAt forest (x, y)
        match thingAt with
        | Some s ->
            match s with
            | Tree -> _move (acc + 1) forest (x + dx, y + dy)
            | OpenSpace -> _move acc forest (x + dx, y + dy)
        | None -> acc

    _move 0 forest (x, y)

// part 1
let forest = forestFromText lines
countTrees forest (0, 0) (3, 1)

// part 2
[ (1, 1)
  (3, 1)
  (5, 1)
  (7, 1)
  (1, 2) ]
|> Seq.map (countTrees forest (0, 0))
|> Seq.map uint64
|> Seq.reduce (fun x y -> x * y)

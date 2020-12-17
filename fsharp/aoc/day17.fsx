open System.IO

// functions changed to do part 2.
// part one can be done by replacing
// the quadruples with triples everywhere
let deltas =
    let mutable lists = []

    for x in -1 .. 1 do
        for y in -1 .. 1 do
            for z in -1 .. 1 do
                for w in -1 .. 1 do
                    match (x, y, z, w) with
                    | (0, 0, 0, 0) -> ()
                    | t -> lists <- t :: lists
    lists

let neighbors (x, y, z, w) =
    deltas
    |> List.map (fun (dx, dy, dz, dw) -> x + dx, y + dy, z + dz, w + dw)

let state =
    File.ReadAllLines "input_17.txt"
    |> Seq.indexed
    |> Seq.collect (fun (y, row) ->
        row
        |> Seq.toList
        |> Seq.indexed
        |> Seq.map (fun (x, cell) ->
            match cell with
            | '#' -> Some(x, y, 0, 0)
            | '.' -> None
            | _ -> failwithf "unexpected cell '%A'" cell))
    |> Seq.choose id
    |> Set.ofSeq

let cycle state =
    let cellsToEvaluate =
        state
        |> Set.toSeq
        |> Seq.collect neighbors
        |> Set.ofSeq
        |> Set.toList

    let countActiveNeighbors xyzw =
        neighbors xyzw
        |> List.filter (fun x -> Set.contains x state)
        |> List.length

    let rec evaluateCells (acc: Set<_>) cells =
        match cells with
        | cell :: tail ->
            match Set.contains cell state with
            | true ->
                match countActiveNeighbors cell with
                | 2
                | 3 -> evaluateCells (acc.Add(cell)) tail
                | _ -> evaluateCells acc tail
            | false ->
                match countActiveNeighbors cell with
                | 3 -> evaluateCells (acc.Add(cell)) tail
                | _ -> evaluateCells acc tail
        | [] -> acc

    evaluateCells Set.empty cellsToEvaluate

state
|> cycle
|> cycle
|> cycle
|> cycle
|> cycle
|> cycle
|> Set.count

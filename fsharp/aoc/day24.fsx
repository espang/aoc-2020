open System.IO

type Direction =
    | East
    | SouthEast
    | SouthWest
    | West
    | NorthWest
    | NorthEast

let parseLine (s: string) =
    //e, se, sw, w, nw, and ne
    let rec l acc cs =
        match cs with
        | 'e' :: tail -> l (East :: acc) tail
        | 'w' :: tail -> l (West :: acc) tail
        | 's' :: 'e' :: tail -> l (SouthEast :: acc) tail
        | 's' :: 'w' :: tail -> l (SouthWest :: acc) tail
        | 'n' :: 'e' :: tail -> l (NorthEast :: acc) tail
        | 'n' :: 'w' :: tail -> l (NorthWest :: acc) tail
        | [] -> List.rev acc
        | _ -> failwithf "can't parse %s" s

    l [] (Seq.toList s)

let steps filename =
    File.ReadAllLines filename |> Array.map parseLine

let move x y d =
    match d with
    | East -> x + 1, y
    | West -> x - 1, y
    | SouthEast -> x, y - 1
    | SouthWest -> x - 1, y - 1
    | NorthEast -> x + 1, y + 1
    | NorthWest -> x, y + 1

let rec moves (x, y) directions =
    match directions with
    | d::tail ->
        moves (move x y d) tail
    | [] -> x, y

// part 1
let instructions = steps "input_24.txt"
instructions
|> Seq.map (moves (0, 0))
|> Seq.countBy id
|> Seq.filter (fun (_, c) -> c % 2 = 1)
|> Seq.length

let deltas =
    [
        (move 0 0 East)
        (move 0 0 West)
        (move 0 0 SouthEast)
        (move 0 0 SouthWest)
        (move 0 0 NorthEast)
        (move 0 0 NorthWest)
    ]

let neighbors (x, y) =
    deltas
    |> List.map (fun (dx, dy) -> x + dx, y + dy)

let cycle state =
    let tilesToEvaluate =
        state
        |> Set.toSeq
        |> Seq.collect neighbors
        |> Set.ofSeq
        |> Set.toList

    let countAdjacentBlackTiles xy =
        neighbors xy
        |> List.filter (fun x -> Set.contains x state)
        |> List.length

    let rec evaluateTiles (acc: Set<_>) cells =
        match cells with
        | cell :: tail ->
            match Set.contains cell state with
            | true ->
                match countAdjacentBlackTiles cell with
                | 1 | 2 -> evaluateTiles (acc.Add(cell)) tail
                | _ -> evaluateTiles acc tail
            | false ->
                match countAdjacentBlackTiles cell with
                | 2 -> evaluateTiles (acc.Add(cell)) tail
                | _ -> evaluateTiles acc tail
        | [] -> acc

    evaluateTiles Set.empty tilesToEvaluate

// part2
let instructions2 = steps "input_t24.txt"
let blackTiles = 
    instructions2
    |> Seq.map (moves (0, 0))
    |> Seq.countBy id
    |> Seq.filter (fun (_, c) -> c % 2 = 1)
    |> Seq.map fst
    |> Set.ofSeq

let rec repeat n tiles = 
    if n = 0
    then tiles
    else
        repeat (n - 1) (cycle tiles)

repeat 100 blackTiles
|> Set.count
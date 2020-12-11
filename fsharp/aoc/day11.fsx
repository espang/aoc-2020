open System.IO

type Position =
    | Empty
    | Occupied
    | Floor

let seats =
    File.ReadAllLines "input_11.txt"
    |> Seq.map (fun s ->
        s
        |> Seq.toList
        |> List.map (fun c -> 
            match c with
            | 'L' -> Empty
            | '#' -> Occupied
            | '.' -> Floor
            | _ -> failwith "invalid seat"))
        |> array2D

let count pos (poss:_[,]) =
    let mutable total = 0
    Array2D.iter (fun p -> if p = pos then total <- total + 1) seats
    total

let deltas = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)]

let isIn (row, col) rows cols =
    (row >= 0 && row < rows)
    && (col >= 0 && col < cols)


let occupiedAround (row, col) (poss:Position [,]) =
    let rows = poss.GetLength(0)
    let cols = poss.GetLength(1)

    deltas
    |> Seq.map (fun (drow, dcol) -> (row + drow, col + dcol))
    |> Seq.filter (fun (r, c) -> isIn (r, c) rows cols && poss.[r, c]=Occupied)
    |> Seq.length

let newStateAt1 (row, col) (poss:_[,]) =
    match poss.[row, col] with
    | Empty ->
        if occupiedAround (row, col) poss = 0
        then Occupied
        else Empty
    | Occupied ->
        if occupiedAround (row, col) poss >= 4
        then Empty
        else Occupied
    | Floor -> Floor

let iterate1 (poss:_[,]) =
    let rows = poss.GetLength(0)
    let cols = poss.GetLength(1)

    Array2D.init rows cols (fun r c -> newStateAt1 (r, c) poss)

let rec iterateUntilStable f (poss:_[,]) =
    let poss' = f poss
    if poss = poss'
    then poss
    else iterateUntilStable f poss'

// part1

printfn "part1: %d" (count Occupied (iterateUntilStable iterate1 seats))
// let final = iterateUntilStable seats

// let mutable total = 0
// for rows in 0..final.GetLength(0)-1 do
//     for cols in 0..final.GetLength(1)-1 do
//         if final.[rows, cols] = Occupied
//         then total <- total + 1
// total

// part 2

let rec anyOccupied (drow, dcol) (row, col) (poss:_[,]) =
    let rows = poss.GetLength(0)
    let cols = poss.GetLength(1)

    let r' = row + drow
    let c' = col + dcol

    if not (isIn (r', c') rows cols)
    then false
    else
        match poss.[r', c'] with
        | Occupied -> true
        | Empty -> false
        | _ -> anyOccupied (drow, dcol) (r',c') poss


let occupiedAround2 (row, col) (poss:_[,]) =
    deltas
    |> List.map (fun d -> anyOccupied d (row, col) poss)
    |> List.filter ((=) true)
    |> List.length

let newStateAt2 (x, y) (poss:_[,]) =
    match poss.[x,y] with
    | Empty ->
        if occupiedAround2 (x,y) poss = 0
        then Occupied
        else Empty
    | Occupied ->
        if occupiedAround2 (x, y) poss >= 5
        then Empty
        else Occupied
    | Floor -> Floor

let iterate2 (poss:_[,]) =
    let rows = poss.GetLength(0)
    let cols = poss.GetLength(1)

    Array2D.init rows cols (fun x y -> newStateAt2 (x, y) poss)

let rec iterateUntilStable2 (poss:_[,]) =
    let poss' = iterate2 poss
    if poss = poss'
    then poss
    else iterateUntilStable2 poss'

let final2 = 
    iterateUntilStable2 seats



let mutable total2 = 0
for rows in 0..final2.GetLength(0)-1 do
    for cols in 0..final2.GetLength(1)-1 do
        if final2.[rows, cols] = Occupied
        then total2 <- total2 + 1
total2


open System.IO

type Seat =
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

let count seatType (seats: _ [,]) =
    let mutable total = 0
    Array2D.iter (fun s -> if s = seatType then total <- total + 1) seats
    total

let deltas =
    [ (-1, -1)
      (-1, 0)
      (-1, 1)
      (0, -1)
      (0, 1)
      (1, -1)
      (1, 0)
      (1, 1) ]

let isIn (row, col) rows cols =
    (row >= 0 && row < rows)
    && (col >= 0 && col < cols)

let occupiedAround (row, col) (seats: _ [,]) =
    let rows = seats.GetLength(0)
    let cols = seats.GetLength(1)

    deltas
    |> Seq.map (fun (drow, dcol) -> (row + drow, col + dcol))
    |> Seq.filter (fun (r, c) -> isIn (r, c) rows cols && seats.[r, c] = Occupied)
    |> Seq.length

let newStateAt1 (seats: _ [,]) row col =
    match seats.[row, col] with
    | Empty -> if occupiedAround (row, col) seats = 0 then Occupied else Empty
    | Occupied -> if occupiedAround (row, col) seats >= 4 then Empty else Occupied
    | Floor -> Floor

let rec iterateUntilStable f (seats: _ [,]) =
    let seats' =
        Array2D.init (seats.GetLength 0) (seats.GetLength 1) (f seats)

    if seats = seats' then seats else iterateUntilStable f seats'

let rec anyOccupied (drow, dcol) (row, col) (seats: _ [,]) =
    let rows = seats.GetLength(0)
    let cols = seats.GetLength(1)

    let r' = row + drow
    let c' = col + dcol

    if not (isIn (r', c') rows cols) then
        false
    else
        match seats.[r', c'] with
        | Occupied -> true
        | Empty -> false
        | _ -> anyOccupied (drow, dcol) (r', c') seats

let occupiedAround2 (row, col) (seats: _ [,]) =
    deltas
    |> List.map (fun d -> anyOccupied d (row, col) seats)
    |> List.filter ((=) true)
    |> List.length

let newStateAt2 (seats: _ [,]) row col =
    match seats.[row, col] with
    | Empty -> if occupiedAround2 (row, col) seats = 0 then Occupied else Empty
    | Occupied -> if occupiedAround2 (row, col) seats >= 5 then Empty else Occupied
    | Floor -> Floor

// part1
printfn "part1: %d" (count Occupied (iterateUntilStable newStateAt1 seats))

// part2
printfn "part2: %d" (count Occupied (iterateUntilStable newStateAt2 seats))

open System.IO

type Instruction =
    | North
    | South
    | West
    | East
    | Left
    | Right
    | Forward

type Direction =
    | N
    | S
    | W
    | E

type Ship = { Dir: Direction; X: int; Y: int }

let left d =
    match d with
    | N -> W
    | W -> S
    | S -> E
    | E -> N

let right d =
    match d with
    | N -> E
    | E -> S
    | S -> W
    | W -> N

let rec turn f s n =
    if n < 0
    then failwith "expected to turn in multiples of 90"
    else if n = 0
    then s
    else turn f { s with Dir = (f s.Dir) } (n - 90)

let rec moveShip s i n =
    match i with
    | North -> { s with Y = s.Y + n }
    | West -> { s with X = s.X - n }
    | South -> { s with Y = s.Y - n }
    | East -> { s with X = s.X + n }
    | Forward ->
        match s.Dir with
        | N -> moveShip s North n
        | W -> moveShip s West n
        | S -> moveShip s South n
        | E -> moveShip s East n
    | Left -> turn left s n
    | Right -> turn right s n

let instructionFrom c =
    match c with
    | 'N' -> North
    | 'W' -> West
    | 'S' -> South
    | 'E' -> East
    | 'F' -> Forward
    | 'R' -> Right
    | 'L' -> Left
    | _ -> failwith "unexpected character"

let instructions =
    File.ReadAllLines "input_12.txt"
    |> Seq.map (fun x -> instructionFrom x.[0], int x.[1..])

let rec applyInstructions ship instructions =
    match instructions with
    | (i, n) :: tail -> applyInstructions (moveShip ship i n) tail
    | [] -> ship


let ship =
    applyInstructions { Dir = E; X = 0; Y = 0 } (Seq.toList instructions)

abs ship.X + abs ship.Y

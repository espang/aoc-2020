open System.IO

type Instruction =
    | North
    | South
    | West
    | East
    | Left
    | Right
    | Forward

type Ship =
    { X: int
      Y: int
      WaypointX: int
      WaypointY: int }

let right (wx, wy) = wy, -wx

let left (wx, wy) = -wy, wx

let rec turn f s n =
    if n < 0 then
        failwith "expected to turn in multiples of 90"
    else if n = 0 then
        s
    else
        let (wx, wy) = f (s.WaypointX, s.WaypointY)
        turn
            f
            { s with
                  WaypointX = wx
                  WaypointY = wy }
            (n - 90)

let rec moveShip s i n =
    match i with
    | North -> { s with WaypointY = s.WaypointY + n }
    | West -> { s with WaypointX = s.WaypointX - n }
    | South -> { s with WaypointY = s.WaypointY - n }
    | East -> { s with WaypointX = s.WaypointX + n }
    | Forward ->
        let newX = s.X + n * s.WaypointX
        let newY = s.Y + n * s.WaypointY
        { s with X = newX; Y = newY }
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
    applyInstructions
        { X = 0
          Y = 0
          WaypointX = 10
          WaypointY = 1 }
        (Seq.toList instructions)

abs ship.X + abs ship.Y

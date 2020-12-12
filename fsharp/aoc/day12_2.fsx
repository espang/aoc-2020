open System.IO

type Instruction =
    | North
    | South
    | West
    | East
    | Left
    | Right
    | Forward

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

type Ship =
    { X: int
      Y: int
      WaypointX: int
      WaypointY: int }

let applyTimes n f args =
    List.fold (fun acc _ -> f acc) args [ 1 .. n ]

let right s =
    { s with
          WaypointX = s.WaypointY
          WaypointY = -s.WaypointX }

let left s =
    { s with
          WaypointX = -s.WaypointY
          WaypointY = s.WaypointX }

let turn f s n =
    if n % 90 <> 0 then
        failwith "expected to turn in multiples of 90"
    else
        applyTimes (n/90) f s

let moveShip s (i, n) =
    match i with
    | North -> { s with WaypointY = s.WaypointY + n }
    | West -> { s with WaypointX = s.WaypointX - n }
    | South -> { s with WaypointY = s.WaypointY - n }
    | East -> { s with WaypointX = s.WaypointX + n }
    | Forward ->
        { s with
              X = s.X + n * s.WaypointX
              Y = s.Y + n * s.WaypointY }
    | Left -> turn left s n
    | Right -> turn right s n

let ship =
    Seq.fold
        moveShip
        { X = 0
          Y = 0
          WaypointX = 10
          WaypointY = 1 }
        instructions

abs ship.X + abs ship.Y

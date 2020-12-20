open System.IO
open System.Collections.Generic

type Tile =
    { ID: int
      // List of 4 elements containing the borders of the tile in following order:
      // [Top Right Bottom Left]. Each side is described as a sorted tuple of two
      // integers.
      // A side like "..##....##" is represented by 0b0011000011
      // The side becomes "##....##.." (or 0b1100001100) when rotated twice.
      // (0b0011000011, 0b1100001100) describes a side uniqly. The left value is
      // always smaller or equal to the right side.
      Edges: (int * int) list
      Body: string [] }

// clockwise rotation of a tile
// * D0 is no rotation
// * D90 is rotation by 90 degress clockwise.
//   The top will be the right side after rotating a tile with D90.
// * D180 is a rotation by 180 degrees clockwise.
//   The top will be the bottom side after rotating a tile with D180.
// * D270 is a rotation by 270 degrees clockwise.
//   The top will be the left side after rotating a tile with D270.
type Rotate =
    | D0
    | D90
    | D180
    | D270

type Side =
    | Right
    | Bottom

type TileType =
    | Corner
    | Border
    | Inner

let inverse (tileEdge: int) =
    System.Convert.ToString(tileEdge, 2).PadLeft(10, '0')
    |> Seq.rev
    |> System.String.Concat
    |> (fun x -> System.Convert.ToInt32(x, 2))

let withInverse (tileEdge: int) =
    let e = inverse tileEdge
    if e < tileEdge then e, tileEdge else tileEdge, e

let tileTypeOf tile (counter: Map<int * int, int>) =
    let nonMatchableEdges =
        tile.Edges
        |> List.map (fun e -> counter.[e])
        |> List.filter ((=) 1)
        |> List.length

    match nonMatchableEdges with
    | 2 -> Corner
    | 1 -> Border
    | _ -> Inner

// This function is used to place a tile into a puzzle. The puzzle is
// solved from top left to bottom right. So a new tile is placed with
// optional left and top tiles. This function returns how the tile
// needs to be flipped and rotated.
let rotateTileToMatch top left tile (counter: Map<int * int, int>) =
    match left with
    | Some tl ->
        match top with
        | Some tt ->
            // Rotate the current tile so that it fits the top and left
            // pieces.
            match tile.Edges with
            | [ le; top; _; _ ] when le = tl && top = tt -> D270, false
            | [ _; le; top; _ ] when le = tl && top = tt -> D180, false
            | [ _; _; le; top ] when le = tl && top = tt -> D90, false
            | [ top; _; _; le ] when le = tl && top = tt -> D0, false
            | [ le; _; _; top ] when le = tl && top = tt -> D90, true
            | [ top; le; _; _ ] when le = tl && top = tt -> D180, true
            | [ _; top; le; _ ] when le = tl && top = tt -> D270, true
            | [ _; _; top; le ] when le = tl && top = tt -> D0, true
            | _ -> failwith "hmm; shouldn't happen when tiles are selected correctly"
        | None ->
            // top row; Rotate and flip the current tile so that it matches
            // the left piece. Handle corner and border pieces separatly.
            // A count of 1 means this side is a border; 2 means this side
            // has a counterpart. 1s have to be facing outside so that the puzzle
            // can be solved.
            match tile.Edges |> List.map (fun t -> counter.[t]) with
            | [ 1; 1; 2; 2 ] -> if tile.Edges.[3] = tl then D0, false else D270, true
            | [ 1; 2; 2; 2 ] -> if tile.Edges.[3] = tl then D0, false else D180, true
            | [ 2; 1; 1; 2 ] -> if tile.Edges.[0] = tl then D270, false else D0, true
            | [ 2; 1; 2; 2 ] -> if tile.Edges.[0] = tl then D270, false else D270, true
            | [ 2; 2; 1; 1 ] -> if tile.Edges.[1] = tl then D180, false else D90, true
            | [ 2; 2; 1; 2 ] -> if tile.Edges.[1] = tl then D180, false else D0, true
            | [ 1; 2; 2; 1 ] -> if tile.Edges.[2] = tl then D90, false else D180, true
            | [ 2; 2; 2; 1 ] -> if tile.Edges.[2] = tl then D90, false else D90, true
            | _ -> failwith "can't rotate given tile"
    | None ->
        match top with
        | Some tt ->
            // first column; Rotate and flip the current tile so that it matches
            // the top piece. Handle corner and border pieces separatly.
            // A count of 1 means this side is a border; 2 means this side
            // has a counterpart. 1s have to be facing outside so that the puzzle
            // can be solved.
            match tile.Edges |> List.map (fun t -> counter.[t]) with
            | [ 1; 1; 2; 2 ] -> if tile.Edges.[2] = tt then D180, false else D90, true
            | [ 2; 1; 2; 2 ] -> if tile.Edges.[2] = tt then D180, false else D180, true
            | [ 2; 1; 1; 2 ] -> if tile.Edges.[3] = tt then D90, false else D180, true
            | [ 2; 2; 1; 2 ] -> if tile.Edges.[3] = tt then D90, false else D270, true
            | [ 2; 2; 1; 1 ] -> if tile.Edges.[0] = tt then D0, false else D270, true
            | [ 2; 2; 2; 1 ] -> if tile.Edges.[0] = tt then D0, false else D0, true
            | [ 1; 2; 2; 1 ] -> if tile.Edges.[1] = tt then D270, false else D0, true
            | [ 1; 2; 2; 2 ] -> if tile.Edges.[1] = tt then D270, false else D90, true
            | _ -> failwith "can't rotate given tile"
        | None ->
            // top left corner. This is the first piece to set. Arrange the piece that
            // the borders face left and top. This is done to support solving the puzzle
            // from the top left to the bottom right.
            match tile.Edges |> List.map (fun t -> counter.[t]) with
            | [ 1; 1; _; _ ] -> D270, false
            | [ _; 1; 1; _ ] -> D180, false
            | [ _; _; 1; 1 ] -> D90, false
            | [ 1; _; _; 1 ] -> D0, false
            | _ -> failwith "can't rotate given tile"

let readInput filename =
    let parseToNumber (cs: char list) =
        cs
        |> Seq.map (fun c ->
            match c with
            | '#' -> '0'
            | '.' -> '1'
            | _ -> failwith "unexpected char")
        |> System.String.Concat
        |> (fun x -> System.Convert.ToInt32(x, 2))

    let parseTile (s: string) =
        let lines = s.Split("\n")

        let top =
            lines.[1]
            |> Seq.toList
            |> parseToNumber
            |> withInverse

        let left =
            lines.[1..]
            |> Seq.map (Seq.head)
            |> Seq.toList
            |> parseToNumber
            |> withInverse

        let right =
            lines.[1..]
            |> Seq.map (Seq.last)
            |> Seq.toList
            |> parseToNumber
            |> withInverse

        let bottom =
            lines
            |> Seq.last
            |> Seq.toList
            |> parseToNumber
            |> withInverse

        { ID = (int (lines.[0].Split().[1].Replace(":", "")))
          Edges = [ top; right; bottom; left ]
          Body = lines.[1..] }

    (File.ReadAllText filename).Split("\n\n")
    |> Seq.map parseTile

let counter tiles =
    tiles
    |> Seq.collect (fun t -> t.Edges)
    |> Seq.countBy id
    |> Map.ofSeq

let find wanted tiles =
    let c = counter tiles
    tiles
    |> Seq.filter (fun t -> wanted = tileTypeOf t c)

// take returns a side (Right or Bottom) of a tile given
// its rotaiton and if it is flipped.
let take side tile (rotation, flipped) =
    let idx =
        match rotation with
        | D0 ->
            match side with
            | Bottom -> if flipped then 0 else 2
            | Right -> 1
        | D90 ->
            match side with
            | Bottom -> 1
            | Right -> if flipped then 2 else 0
        | D180 ->
            match side with
            | Bottom -> if flipped then 2 else 0
            | Right -> 3
        | D270 ->
            match side with
            | Bottom -> 3
            | Right -> if flipped then 0 else 2

    tile.Edges.[idx]

let solvePuzzle n tiles =
    let startWith = find Corner tiles |> Seq.head
    let c = counter tiles
    let rotation = rotateTileToMatch None None startWith c

    let tileById =
        tiles |> Seq.map (fun t -> t.ID, t) |> Map.ofSeq

    let seen = new HashSet<int>()

    let findTileWith edge =
        tiles
        |> Seq.filter (fun t -> seen.Contains t.ID |> not)
        |> Seq.filter (fun t -> List.contains edge t.Edges)
        |> Seq.head

    let puzzle: (int * (Rotate * bool)) [,] =
        Array2D.init n n (fun _ _ -> 0, (D0, false))

    puzzle.[0, 0] <- startWith.ID, rotation
    seen.Add startWith.ID |> ignore

    let rec solve row col =
        let left =
            if col > 0 then
                let tid, r = puzzle.[row, col - 1]
                Some(take Right tileById.[tid] r)
            else
                None

        let top =
            if row > 0 then
                let tid, r = puzzle.[row - 1, col]
                Some(take Bottom tileById.[tid] r)
            else
                None

        let next =
            match left with
            | Some l -> findTileWith l
            | None ->
                match top with
                | Some t -> findTileWith t
                | None -> failwith "never here! This should only happen after 1 tile is placed. Therefore each following tile should be connected to the other puzzles!"

        let rotation = rotateTileToMatch top left next c
        puzzle.[row, col] <- next.ID, rotation
        seen.Add next.ID |> ignore
        if col = n - 1
        then if row = n - 1 then () else solve (row + 1) 0
        else solve row (col + 1)

    solve 0 1
    puzzle

let rotateLeft (board: _ [,]) =
    let nRows = board.GetLength(0)
    let nCols = board.GetLength(1)
    Array2D.init nRows nCols (fun row col -> board.[col, nRows - 1 - row])

let flip (board: _ [,]) =
    let nRows = board.GetLength(0)
    let nCols = board.GetLength(1)
    Array2D.init nRows nCols (fun row col -> board.[nRows - 1 - row, col])

let rotate (r, shallFlip) (board: _ [,]) =
    let op b = if shallFlip then flip b else b
    match r with
    | D0 -> board |> op
    | D90 ->
        board
        |> op
        |> rotateLeft
        |> rotateLeft
        |> rotateLeft
    | D180 -> board |> op |> rotateLeft |> rotateLeft
    | D270 -> board |> op |> rotateLeft

let puzzleToText tiles (puzzle: (int * (Rotate * bool)) [,]) =
    let n = puzzle.GetLength(0)

    let tileById =
        tiles |> Seq.map (fun t -> t.ID, t) |> Map.ofSeq

    // Each tile is 8x8 chars. Create the board:
    let board = Array2D.init (n * 8) (n * 8) (fun _ _ -> ' ')

    seq {
        for row in 0 .. n - 1 do
            for col in 0 .. n - 1 -> row, col
    }
    |> Seq.iter (fun (r, c) ->
        let id, rotation = puzzle.[r, c]
        let subBoard = rotate rotation (tileById.[id].Body |> array2D)
        seq {
            // the subboard is 10x10; but here only the inner 8x8 are used.
            for irow in 0 .. 7 do
                for icol in 0 .. 7 -> irow, icol
        }
        |> Seq.iter (fun (ir, ic) ->
            let row = r * 8 + ir
            let col = c * 8 + ic
            // increase the subBoard's indexes by 1 to ignore the border
            // this handles only the inner 8x8 of the total 10x10 chars
            board.[row, col] <- subBoard.[ir + 1, ic + 1]))
    board

//                  #
//#    ##    ##    ###
// #  #  #  #  #  #
//12345678901234567890
let seaMonster =
    [ 0, 0
      1, 1
      1, 4
      0, 5
      0, 6
      1, 7
      1, 10
      0, 11
      0, 12
      1, 13
      1, 16
      0, 17
      -1, 18
      0, 18
      0, 19 ]

let findSeaMonsters (board: char [,]) =
    let nRows = board.GetLength(0)
    let nCols = board.GetLength(1)

    let seaMonsterAt (row, col) =
        seaMonster
        |> Seq.forall (fun (dr, dc) -> board.[row + dr, col + dc] = '#')

    seq {
        for row in 1 .. nRows - 2 do
            for col in 0 .. nCols - 20 - 1 -> row, col
    }
    |> Seq.filter seaMonsterAt

let replaceSeaMonsters (board: char [,]) =
    let markSeaMonsterAt (row, col) =
        seaMonster
        |> Seq.iter (fun (dr, dc) -> board.[row + dr, col + dc] <- 'O')

    findSeaMonsters board |> Seq.iter markSeaMonsterAt
    board

let printBoard (board: char [,]) =
    seq { for row in 0 .. board.GetLength(0) - 1 -> row }
    |> Seq.iter (fun r -> printfn "%s" (board.[r, *] |> System.String.Concat))

// part 1
readInput "input_20.txt"
|> find Corner
|> Seq.fold (fun acc (x: Tile) -> acc * (bigint (x.ID))) (bigint 1)

// part 2
let tiles = readInput "input_20.txt"
let board =
    solvePuzzle 12 tiles |> puzzleToText tiles

// manually rotate until a board has monsters
let boardWithMonsters = board |> rotateLeft |> rotateLeft
// count '#' ignoring monsters:
let mutable total = 0
Array2D.iter (fun c -> if c = '#' then total <- total + 1) (replaceSeaMonsters boardWithMonsters)
total

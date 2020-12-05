open System.IO

let rowNumber cs =
    Seq.rev cs
    |> Seq.map (fun c -> if c = 'B' then 1 else 0)
    |> Seq.mapi (fun i v -> v <<< i)
    |> Seq.sum

let columnNumber cs =
    Seq.rev cs
    |> Seq.map (fun c -> if c = 'R' then 1 else 0)
    |> Seq.mapi (fun i v -> v <<< i)
    |> Seq.sum

let seatID r c = 8 * r + c

let seatIDfromString s =
    let row = rowNumber (Seq.take 7 s)
    let col = columnNumber (Seq.skip 7 s)
    seatID row col

//part1
File.ReadAllLines "input_5.txt"
|> Seq.maxBy seatIDfromString
|> seatIDfromString

//part2
let seats =
    File.ReadAllLines "input_5.txt"
    |> Seq.map seatIDfromString

seats
|> Seq.sort
|> Seq.pairwise
|> Seq.filter (fun (x, y) -> (y - x) = 2)

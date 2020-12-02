open System.IO

let lines =
    File.ReadAllLines "./fsharp/aoc/input_2.txt"

type CharacterRange = { From: int; To: int }

let Contains range v = v >= range.From && v <= range.To

let parseFirstEntry (entry: string) =
    let splitted = entry.Split("-")
    { From = int splitted.[0]
      To = int splitted.[1] }

// parseFirstEntry "1-2"

let parseCharacter (entry: string) = entry.[0]

let parseLine (line: string) =
    let entries = line.Split(" ")
    (parseFirstEntry entries.[0], parseCharacter entries.[1], entries.[2])

let isValid (range, c, password) =
    let cCount =
        password
        |> Seq.filter (fun x -> x = c)
        |> Seq.length

    Contains range cCount

lines
|> Seq.map parseLine
|> Seq.filter isValid
|> Seq.length

let isValid2 (range, c, password: string) =
    let c1 = password.[range.From]
    let c2 = password.[range.To]
    c = c1 && c <> c2 || c <> c1 && c = c2

lines
|> Seq.map parseLine
|> Seq.filter isValid2
|> Seq.length

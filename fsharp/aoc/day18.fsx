open System
open System.IO

let charToInt c = uint64 ((int c) - 48)

let rec evalLine (line: char list) =
    if line.IsEmpty then
        failwith "can't evaluate an empty line"
    else
        let rec _evalLine acc operator line =
            match line with
            | '(' :: tail ->
                let (value, rest) = evalLine tail
                _evalLine (operator acc value) operator rest
            | ')' :: tail -> acc, tail
            | '*' :: tail -> _evalLine acc (*) tail
            | '+' :: tail -> _evalLine acc (+) tail
            | d :: tail when Char.IsDigit d -> _evalLine (operator acc (charToInt d)) operator tail
            | ' ' :: tail -> _evalLine acc operator tail
            | [] -> acc, []
            | c :: _ -> failwithf "unexpected char %A" c


        _evalLine (uint64 0) (+) line

// part 1
File.ReadAllLines "input_18.txt"
|> Seq.filter (fun line -> line <> "")
|> Seq.sumBy (Seq.toList >> evalLine >> fst)

open System.IO

let groups =
    (File.ReadAllText "input_6.txt").Split("\n\n")

let anyYesAnswerInGroup s =
    s |> Seq.filter System.Char.IsLetter |> Set.ofSeq

printfn
    "part1: %d"
    (groups
     |> Array.sumBy (anyYesAnswerInGroup >> Set.count))

let everyYesAnswerInGroup (s: string) =
    let ss = s.Trim().Split("\n")
    let answerSets = ss |> Array.map anyYesAnswerInGroup
    answerSets
    |> Array.fold Set.intersect (Array.head answerSets)

printfn
    "part2: %d"
    (groups
     |> Array.sumBy (everyYesAnswerInGroup >> Set.count))

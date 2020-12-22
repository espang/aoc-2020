open System.IO

let decks =
    let items =
        (File.ReadAllText "input_22.txt").Split("\n\n")
        |> Array.map
            (fun s ->
                s.Split("\n")
                |> Array.skip 1
                |> Array.map int
                |> Array.toList)

    items.[0], items.[1]

let testd1, testd2 = [ 9; 2; 6; 3; 1 ], [ 5; 8; 4; 7; 10 ]
let deck1, deck2 = decks

let score deck =
    deck
    |> List.rev
    |> List.indexed
    |> List.fold (fun acc (index, value) -> acc + (index + 1) * value) 0

let playGame deck1 deck2 =
    let turn deck1 deck2 =
        match deck1 with
        | c1 :: d1 ->
            match deck2 with
            | c2 :: d2 -> if c1 > c2 then Some(d1 @ [ c1; c2 ], d2) else Some(d1, d2 @ [ c2; c1 ])
            | [] -> None
        | [] -> None

    let rec next d1 d2 =
        match turn d1 d2 with
        | Some (d1', d2') -> next d1' d2'
        | None -> if List.isEmpty d1 then d2 else d1

    next deck1 deck2

let wd = playGame deck1 deck2
score wd

type Winner =
    | Player1
    | Player2

let rec playRecGame deck1 deck2 =
    let turn deck1 deck2 =
        match deck1 with
        | c1 :: d1 ->
            match deck2 with
            | c2 :: d2 ->
                if d1.Length >= c1 && d2.Length >= c2 then
                    match playRecGame (List.take c1 d1) (List.take c2 d2) with
                    | Player1, _ -> Some(d1 @ [ c1; c2 ], d2)
                    | Player2, _ -> Some(d1, d2 @ [ c2; c1 ])
                else if c1 > c2 then
                    Some(d1 @ [ c1; c2 ], d2)
                else
                    Some(d1, d2 @ [ c2; c1 ])
            | [] -> None
        | [] -> None

    let rec next (seen: Set<_>) d1 d2 =
        if seen.Contains(d1, d2) then
            Player1, d1
        else
            match turn d1 d2 with
            | Some (d1', d2') -> next (seen.Add((d1, d2))) d1' d2'
            | None -> if List.isEmpty d1 then Player2, d2 else Player1, d1

    next Set.empty deck1 deck2

let _, wd2 = playRecGame deck1 deck2
score wd2

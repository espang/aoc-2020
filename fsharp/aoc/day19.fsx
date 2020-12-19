open System.IO
open System.Collections.Generic

type Tuple =
    | E of char
    | S of int
    | T of int * int
    | T3 of int * int * int

let intListFromTuple t =
    match t with
    | E _ -> failwith "no"
    | S i -> [ i ]
    | T (i, j) -> [ i; j ]
    | T3 (i, j, k) -> [ i; j; k ]

let parseRules (s: string) =
    s.Split "\n"
    |> Seq.map (fun line ->
        let items =
            line.Split(":") |> Array.map (fun x -> x.Trim())

        let key = int items.[0]

        let values =
            match items.[1] with
            | "\"a\"" -> [ E 'a' ]
            | "\"b\"" -> [ E 'b' ]
            | tuples ->
                (Seq.map
                    ((fun (s: string) -> s.Trim())
                     >> (fun (t: string) ->
                         let numbers = t.Split() |> Array.map int
                         match numbers.Length with
                         | 1 -> S(numbers.[0])
                         | 2 -> T(numbers.[0], numbers.[1])
                         | 3 -> T3(numbers.[0], numbers.[1], numbers.[2])
                         | _ -> failwith "only support 1 or 2 numbers"))
                     (tuples.Split("|")))
                |> Seq.toList

        key, values)
    |> Map.ofSeq

let parseMessages (s: string) = s.Split("\n")

let (rules, messages) =
    let items =
        (File.ReadAllText "input_19.txt").Split("\n\n")

    parseRules items.[0], parseMessages items.[1]

let endRules =
    rules
    |> Map.fold (fun (acc: Map<_, _>) k v ->
        match v with
        | [ (E c) ] -> acc.Add(k, c)
        | _ -> acc) Map.empty

let intListToMessage (l: int list) (endRules: Map<int, char>) =
    l
    |> List.map (fun i ->
        match endRules.TryFind i with
        | Some c -> c
        | None -> failwith "intListToMessage")
    |> List.toArray
    |> System.String.Concat

let allChanges l (rules: Map<_, _>) =
    match l with
    | head :: tail ->
        match rules.TryFind head with
        | Some coll -> Some(List.map (intListFromTuple >> (fun t -> t @ tail)) coll)
        | None -> None
    | [] -> None

let findAllMessagesWithLengthBelow length start rules endrules =
    let rules' =
        endrules
        |> Map.fold (fun (acc: Map<_, _>) k _ -> acc.Remove k) rules

    let finals = HashSet<string>()
    let mutable iterations = 0

    let rec grow (prefix: _ list) (l: _ list) =
        iterations <- iterations + 1
        if iterations % 1000 = 0
        then printfn "found %d so far after %d iterations" finals.Count iterations

        if prefix.Length + l.Length > length then
            ()
        else
            match allChanges l rules' with
            | Some coll ->
                for l' in coll do
                    grow prefix l'
            | None ->
                match l with
                | head :: tail -> grow (head :: prefix) tail
                | [] ->
                    finals.Add(intListToMessage (List.rev prefix) endRules)
                    |> ignore
                    ()

    grow [] start
    finals

let l =
    messages |> Seq.map (fun s -> s.Length) |> Seq.max
// part 1
//let allValid = findAllMessagesWithLengthBelow (l + 1) (rules.[0] |> List.head |> intListFromTuple) rules endRules
// messages
// |> Array.filter (allValid.Contains)
// |> Array.length



// part 2

// S -> 8 11
// 8 -> 42 | 42 8
// 11 -> 42 31 | 42 11 31

let potential31s =
    findAllMessagesWithLengthBelow 100 [ 31 ] rules endRules

let potential42s =
    findAllMessagesWithLengthBelow 100 [ 42 ] rules endRules
// intersection is empty!!!

let validateMessage (v42s: HashSet<string>) (v31s: HashSet<string>) (m: string) =
    let rec count c42 c31 check42 (s: string) =
        if s = "" then
            Some(c42, c31)
        else
            let rest = s.Substring(8)
            let test = s.[..7]
            if check42 then
                match v42s.Contains test with
                | true -> count (c42 + 1) c31 true rest
                | false -> count c42 c31 false s
            else
                match v31s.Contains test with
                | true -> count c42 (c31 + 1) false rest
                | false -> None

    if m.Length % 8 <> 0 then
        false
    else
        match count 0 0 true m with
        | Some (c42, c31) -> c31 >= 1 && c42 > c31
        | None -> false

messages
|> Seq.filter (validateMessage potential42s potential31s)
|> Seq.length

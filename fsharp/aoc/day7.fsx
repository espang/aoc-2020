open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let getColour s =
    match s with
    | Regex @"([\w\s]*) bag.*" [ color ] -> color
    | _ -> failwith (sprintf "can't get colour out of '%s'" s)

let getCountAndColour s =
    match s with
    | Regex @"(\d*) ([a-z\s]*) bag.*" [ count; colour ] -> (int count, colour)
    | _ -> failwith (sprintf "can't get count and colour out of '%s'" s)

let hasNoBags s = s = " no other bags."

let getContent (s: string) =
    let splitted = s.Split(",")
    match splitted.Length with
    | 1 when (hasNoBags splitted.[0]) -> []
    | _ -> Array.fold (fun acc s -> (getCountAndColour s) :: acc) List.empty splitted

let parseRawLine (line: string) =
    let splitted = line.Split("contain")
    if splitted.Length = 2
    then (getColour splitted.[0], getContent splitted.[1])
    else failwith "each line should contain 'contain' exactly once"

let colourToContent =
    File.ReadAllLines "input_7.txt"
    |> Seq.filter (fun x -> x.Length > 0)
    |> Seq.map parseRawLine
    |> Map.ofSeq

let search (pred: Set<string> -> Set<string> -> (int * string) list -> bool option) colourMap =
    let colours =
        Map.fold (fun acc k _ -> Set.add k acc) Set.empty colourMap

    let rec loop yes no (unknown: Set<string>) =
        if unknown.IsEmpty then
            yes, no
        else
            let mutable yes' = yes
            let mutable no' = no
            let mutable unknown' = unknown
            for colour in unknown do
                match pred yes no (colourMap.[colour]) with
                | Some true ->
                    yes' <- Set.add colour yes'
                    unknown' <- Set.remove colour unknown'
                | Some false ->
                    no' <- Set.add colour no'
                    unknown' <- Set.remove colour unknown'
                | None -> ()
            if unknown.Count = unknown'.Count then failwith "can't solve the pred" else loop yes' no' unknown'

    loop Set.empty Set.empty colours

let goldPred yes no countColours =
    let goldens =
        countColours
        |> Seq.filter (fun (_, c) -> (Set.contains c yes) || (c = "shiny gold"))

    if Seq.isEmpty goldens then
        if Seq.forall (fun (_, c) -> Set.contains c no) countColours
        then Some false
        else None
    else
        Some true

// part 1
let (canContainGold, _) = search goldPred colourToContent
Set.contains "shiny gold" canContainGold
canContainGold.Count

let countBags colour (colourContent: Map<string, (int * string) list>) =
    let mutable memory = Map.empty

    let rec loop colour =
        if memory.ContainsKey colour then
            memory.[colour]
        else
            let content = colourContent.[colour]

            let bags =
                List.fold (fun acc (n, c) -> (acc + n * (loop c))) 1 content

            memory <- memory.Add(colour, bags)
            bags

    loop colour

// part 2
countBags "shiny gold" colourToContent


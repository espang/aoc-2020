open System.IO

let (rawData, rawMyTicket, rawOtherTickets) =
    let items =
        (File.ReadAllText "input_16.txt").Split("\n\n")

    items.[0], items.[1], items.[2]


let parseDataRow (row: string) =
    let items1 = row.Split(":")
    let location = items1.[0]
    let items2 = items1.[1].Split(" or ")

    let range1 =
        items2.[0].Trim().Split("-") |> Array.map int

    let range2 =
        items2.[1].Trim().Split("-") |> Array.map int

    location, (range1.[0], range1.[1]), (range2.[0], range2.[1])

let data =
    rawData.Split("\n") |> Array.map parseDataRow

let myTicket =
    let items = rawMyTicket.Split("\n")
    items.[1].Split(",") |> Array.map int

let otherTickets =
    let items = rawOtherTickets.Trim().Split("\n")
    items.[1..]
    |> Array.map (fun x -> x.Split(",") |> Array.map int)

let validNumber data number =
    let rec any data =
        match data with
        | (_, (l1, u1), (l2, u2)) :: tail ->
            if l1
               <= number
               && number <= u1
               || l2 <= number && number <= u2 then
                true
            else
                any tail
        | [] -> false

    any data


let part1 data (tickets: int [] []) =
    let notValid number = not (validNumber data number)

    let rec loop acc tickets =
        match tickets with
        | ticket :: tail ->
            loop
                (acc
                 @ (ticket |> Array.filter notValid |> Array.toList))
                tail
        | [] -> acc

    let invalidValues = loop [] (Array.toList tickets)
    List.sum invalidValues

part1 (Array.toList data) otherTickets

let validTickets =
    otherTickets
    |> Array.filter (fun ticket -> Array.forall (validNumber (Array.toList data)) ticket)

let columnValidFor data (validTickets: int [] []) =
    let numberTickets = validTickets.Length
    let numberValues = validTickets.[0].Length

    let columns =
        Array.init numberValues (fun i -> Array.init numberTickets (fun j -> validTickets.[j].[i]))

    let validFields column =
        data
        |> Array.filter (fun (_, (l1, u1), (l2, u2)) ->
            Array.forall (fun n -> l1 <= n && n <= u1 || l2 <= n && n <= u2) column)
        |> Array.map (fun (name, _, _) -> name)

    columns
    |> Array.indexed
    |> Array.fold (fun (acc: Map<int, _>) (i, column) -> acc.Add(i, validFields column)) Map.empty

let search (options: Map<int, string []>) =
    let rec _search (solution: Map<string, int>) =
        let mutable solution' = solution
        for option in options do
            let values =
                option.Value
                |> Array.filter (solution.ContainsKey >> not)

            if values.Length = 1
            then solution' <- solution'.Add(Array.head values, option.Key)
        if solution.Count = solution'.Count
        then if options.Count = solution'.Count then solution' else failwith "no solution found"
        else _search solution'

    _search Map.empty

columnValidFor data validTickets
|> search
|> Map.toList
|> List.filter (fun (x, _) -> x.StartsWith "departure")
|> List.map (fun (_, index) -> uint64 myTicket.[index])
|> List.fold (*) (uint64 1)

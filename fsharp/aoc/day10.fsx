open System.IO
#time
let numbers =
    File.ReadAllLines "input_10.txt"
    |> Seq.map int

let builtInAdapdterJoltage =
    3 + Seq.max numbers

let allNumbers =
    0::builtInAdapdterJoltage::(Seq.toList numbers)

let differences =
    allNumbers
    |> List.sort
    |> List.pairwise
    |> List.map (fun (x, y) -> y - x)

// part 1
(differences |> List.filter (fun x -> x = 1) |> List.length)
* (differences |> List.filter (fun x -> x = 3) |> List.length)


let numberCombinations (connectors:int list) =
    let mutable counter = Map.empty
    let get key =
        match counter.TryFind key with
        | Some v -> v 
        | None -> bigint 0
    let increaseBy key v=
        counter <- counter.Add(key, v + get key)
    
    let rec nextFew n jolt amount connectors =
        if n = 0
        then ()
        else
            match connectors with
            | head::tail -> 
                if head <= jolt + 3
                then 
                    increaseBy head amount
                    nextFew (n-1) jolt amount tail
                else ()
            | [] -> ()

    let rec loop current connectors =
        nextFew 3 current (get current) connectors
        match connectors with
        | head::tail ->
            loop head tail
        | [] -> ()   

    increaseBy connectors.[0] (bigint 1)
    loop connectors.[0] connectors.[1..]
    counter

// part2
let countWaysDict =
    numberCombinations (List.sort allNumbers)

printfn "part: %A" countWaysDict.[builtInAdapdterJoltage]

#time

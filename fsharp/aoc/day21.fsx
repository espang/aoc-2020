open System.IO

let input = 
    File.ReadAllLines "input_21.txt"
    |> Seq.filter (fun s -> s <> "")
    |> Seq.map (fun line ->
        let items = line.Split("(")
        let ingredients = items.[0].Trim().Split()
        let allergens = items.[1].Replace(")","").Replace(",","").Split()
        Set.ofArray ingredients, Set.ofArray allergens.[1..])

let accumulatePotentialIngredients (m:Map<_,_>) food =
    let rec loop (acc:Map<_,_>) allergens =
        match allergens with
        | allergen::tail ->
            match acc.TryFind allergen with
            | Some s ->
                loop (acc.Add(allergen, Set.intersect (fst food) s)) tail
            | None ->
                loop (acc.Add(allergen, (fst food))) tail
        | [] -> acc
    let ret = loop m (Set.toList (snd food))
    ret

let foods = input
let allIngredients =
    foods
    |> Seq.map fst
    |> Seq.fold (fun acc x -> Set.union x acc) Set.empty
    
let allAllergens =
    foods
    |> Seq.map snd
    |> Seq.fold (fun acc x -> Set.union x acc) Set.empty
    
let allergensToPotentialIngredients =
    foods
    |> Seq.fold (accumulatePotentialIngredients) Map.empty

let ingredientsWithoutAllergens =
    allergensToPotentialIngredients
    |> Map.fold (fun acc k v ->
        Set.difference acc v) allIngredients

// part 1
foods
|> Seq.sumBy (fun t ->
    (fst t)
    |> Set.intersect ingredientsWithoutAllergens
    |> Set.count)

// part 2
let allergensToPotentialIngredientsFiltered =
    allergensToPotentialIngredients
    |> Map.fold (fun (m:Map<_,_>) k v ->
        m.Add(k, Set.difference v ingredientsWithoutAllergens)) Map.empty

let resolve (m:Map<_, _>) =
    let rec loop (found:Set<_>) (newMap:Map<_,_>) (oldMap:Map<_,_>) =
        if oldMap.IsEmpty 
        then
            newMap
        else
            match oldMap
                |> Map.tryPick (fun k v ->
                    let diff = Set.difference v found
                    if diff.Count = 1
                    then Some (k, diff |> Set.toSeq |> Seq.head)
                    else None) with
            | Some (k, v) ->
                loop (found.Add v) (newMap.Add(k, v)) (oldMap.Remove(k))
            | None -> failwith "can't solve it"
    loop Set.empty Map.empty m

let ingredientsPart2 =
    resolve allergensToPotentialIngredientsFiltered
    |> Map.toSeq
    |> Seq.sort
    |> Seq.map snd
    |> Seq.toArray

System.String.Join (",", ingredientsPart2)

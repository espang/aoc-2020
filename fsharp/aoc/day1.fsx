open System.IO

let numbers = Seq.map int (File.ReadAllLines "input_1.txt") |> Seq.toList

let findPair numbers sum =
    let rec _findPair acc numbers =
        match numbers with
            | head :: tail ->
                if Set.contains head acc then
                    Some head
                else
                    _findPair (Set.add (sum - head) acc) tail
            | [] -> None
    
    _findPair Set.empty numbers


findPair numbers 2020
// answer part 1:
775 * (2020 - 775)

(List.sort numbers).[..2]
// 201, 534 are the two smallest numbers.
2020  - 201 - 534
// The third summand has to be smaller or equal to 1285
let candidates = List.filter (fun x -> x <= 1285) numbers

let findTriple numbers sum =
    let snumbers = Set.ofList numbers
    let rec _findTriple numbers =
        match numbers with 
        | head :: tail ->
          let pair = findPair (Set.toList (Set.remove head snumbers)) (sum - head) 
          match pair with
            | Some result ->
                let third = sum - head - result
                Some (result * head * third)
            | None ->
                _findTriple tail
        | [] -> None
    _findTriple numbers

// returns the product of the 3 summands that sum up to 2020
findTriple candidates 2020

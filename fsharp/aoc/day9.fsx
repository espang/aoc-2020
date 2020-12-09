open System.IO

type Ring =
    { N: int
      Arr: uint64 []
      Head: int
      Next: int
      Count: int }

let makeRing n =
    { N = n
      Arr = Array.zeroCreate n
      Head = 0
      Next = 0
      Count = 0 }

let isEmpty r = r.Count = 0

let length r = r.Count

let pop r =
    r.Arr.[r.Head],
    { r with
          Head = (r.Head + 1) % r.N
          Count = r.Count - 1 }

let add v r =
    let arr = r.Arr
    arr.[r.Next] <- v
    { r with
          Arr = arr
          Next = (r.Next + 1) % r.N
          Count = r.Count + 1 }

let numbers =
    File.ReadAllLines "input_9.txt" |> Seq.map uint64

let findPairIn arr number =
    let rec find acc arr =
        match arr with
        | h :: t -> if Set.contains h acc
                    then true
                    else find (Set.add (number-h) acc) t
        | [] -> false

    find Set.empty (Array.toList arr)

let findFirst (preambleLength: int) numbers =
    let rec loop (ring: Ring) numbers =
        match numbers with
        | next :: rest ->
            if (length ring) < preambleLength then loop (ring |> add next) rest
            else if findPairIn ring.Arr next then loop (ring |> pop |> snd |> add next) rest
            else Some next
        | [] -> None

    loop (makeRing preambleLength) numbers

let numberPart1 =
    findFirst 25 (Seq.toList numbers)

let findSumTo (number:uint64) (numbers:uint64 []) =
    let rec loop acc from endIndex coll =
        if acc > number
        then
            loop (acc - numbers.[from]) (from + 1) endIndex coll
        else
            if acc = number
            then
                from, endIndex
            else 
                match coll with
                | h::t ->
                    loop (acc + h) from (endIndex + 1) t
                | [] -> failwith "no contigous set found"
    loop (uint64 0) 0 0 (Array.toList numbers)

// find indexes
findSumTo numberPart1.Value (Seq.toArray numbers)
let contSet = (Seq.toArray numbers).[407..424]
// part 2
Array.min contSet
+ Array.max contSet
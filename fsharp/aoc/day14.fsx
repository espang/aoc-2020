open System.IO
open System.Text.RegularExpressions

let numberToDecimalString (x: int) = System.Convert.ToString(int64 x, 2)

let numberExtend l (s: string) =
    if s.Length < l then String.replicate (l - s.Length) "0" + s else s

let applyBitmask bitmask value =
    let value' =
        value
        |> numberToDecimalString
        |> numberExtend 36
        |> Seq.toList
        |> List.zip (Seq.toList bitmask)
        |> List.map (fun (bm, digit) ->
            match bm with
            | 'X' -> digit
            | _ -> bm)
        |> Array.ofList
        |> System.String.Concat

    System.Convert.ToUInt64(value', 2)

let lines = File.ReadAllLines "input_14.txt"

let parseLine (line: string) =
    let items = line.Split()
    let number = Regex "mem\[(\d*)\]"
    if items.[0] = "mask" then
        true, 0, items.[2].Trim()
    else
        let v =
            (number.Match items.[0]).Groups.[1].Value

        false, int v, items.[2].Trim()

let handleAllLines lines =
    let mutable mem = Map.empty

    let rec loop bm lines =
        match lines with
        | l :: tail ->
            match parseLine l with
            | true, _, bm -> loop bm tail
            | false, pos, value ->

                let value' = applyBitmask bm (int value)
                mem <- mem.Add(pos, value')
                loop bm tail
        | [] -> ()

    loop "" lines
    mem

let memory = handleAllLines (Array.toList lines)

//part 1
Map.fold (fun acc k v -> acc + v) (uint64 0) memory

let addresses (bm: string) address =
    let bitmaskLength = bm.Length
    let mutable memAddrs = Set.empty

    let rec loop acc bm =
        match bm with
        | ('X', _) :: tail ->
            loop ('1' :: acc) tail
            loop ('0' :: acc) tail
        | ('0', original) :: tail -> loop (original :: acc) tail
        | ('1', _) :: tail -> loop ('1' :: acc) tail
        | head :: tail -> failwithf "unexpected tuple %A" head
        | [] ->
            let value' =
                List.rev acc
                |> Array.ofList
                |> System.String.Concat

            memAddrs <- memAddrs.Add(System.Convert.ToUInt64(value', 2))

    loop
        []
        (address
         |> numberToDecimalString
         |> numberExtend bitmaskLength
         |> Seq.toList
         |> List.zip (Seq.toList bm))
    memAddrs

let handleAllLines2 lines =
    let mutable mem = Map.empty

    let rec loop bm lines =
        match lines with
        | l :: tail ->
            match parseLine l with
            | true, _, bm -> loop bm tail
            | false, pos, value ->

                let newAddresses = addresses bm (int pos)
                for addr in newAddresses do
                    mem <- mem.Add(addr, uint64 value)
                loop bm tail
        | [] -> ()

    loop "" lines
    mem

let memory2 = handleAllLines2 (Array.toList lines)
//part 2
Map.fold (fun acc k v -> acc + v) (uint64 0) memory2

open System.Collections.Generic

let nthNumberSpoken n (numbers: int list) =
    let lastIndex = Dictionary()

    let rec speak step lastNbr =
        if step > n then
            lastNbr
        else if lastIndex.ContainsKey lastNbr then
            let next = step - 1 - lastIndex.[lastNbr]
            lastIndex.[lastNbr] <- step - 1
            speak (step + 1) next
        else
            lastIndex.[lastNbr] <- step - 1
            speak (step + 1) 0

    for index, number in List.indexed numbers do
        lastIndex.[number] <- index + 1
    speak (1 + numbers.Length) (List.last numbers)

let input = [ 5; 2; 8; 16; 18; 0; 1 ]

printfn "part1: %d" (nthNumberSpoken 2020 input)
printfn "part2: %d" (nthNumberSpoken 30000000 input)

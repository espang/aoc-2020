open System.IO

let earliestTime = 1000417

let firstAfter busId earliestTime =
    if earliestTime % busId = 0
    then earliestTime
    else ((earliestTime / busId) + 1) * busId 

let scoreForPart1 earliestTime (busId, departAt) =
    busId * (departAt - earliestTime)

(Seq.map (int >> (fun x -> x, firstAfter x earliestTime)) ((File.ReadAllText "input_13.txt").Split ","
|> Seq.filter (fun x -> x <> "x")))
|> Seq.sortBy snd
|> Seq.head
|> scoreForPart1 earliestTime

let findFirst (divider:bigint) (rem:bigint) (factor:bigint)=
    let rec loop f2 =
        let rem' = factor * f2 % divider
        if rem' = rem
        then f2
        else
        if rem' > factor
        then
            if rem' > rem 
            then
                //jump to the divider
                let jump = (divider - rem') / factor
                loop (f2 + if jump > bigint 0 then jump else bigint 1)
            else if rem / rem' > (bigint 1)
            then
                // jump to the remainder
                let jump = (rem - rem') / factor 
                loop (f2 + if jump > bigint 0 then jump else bigint 1)
            else
                loop (f2 + (bigint 1))
        else
            loop (f2 + (bigint 1))
    loop (bigint 1)

let findTimestamp (busIds:(bigint*bigint) list) =
    let rec loop (b, m) busIds =
        match busIds with
        | (b',m')::tail ->
            let b2 = findFirst m (b + b') m'
            loop (b2 * m' - b', m' * m) tail
        | [] -> b
    loop busIds.[0] busIds.[1..]

(File.ReadAllText "input_13.txt").Split ","
|> Seq.mapi (fun i x -> i, x)
|> Seq.filter (fun x -> (snd x) <> "x")
|> Seq.map (fun (i, x) -> (bigint i, bigint (int x)))
|> Seq.toList
|> findTimestamp

type Node =
    { Value: int
      Last: Node option Ref
      Next: Node option Ref }

let mustNextCup n =
    match !n.Next with
    | Some next -> next
    | None -> failwith "Next was None"

let makeNode v =
    { Value = v
      Last = ref None
      Next = ref None }

let deckFrom (s: string) =
    let numbers =
        s |> Seq.map (fun c -> int c - 48) |> Seq.toList

    let rec build acc ns =
        match ns with
        | head :: tail ->
            let last = List.head acc
            let node = makeNode head
            last.Next := Some node
            node.Last := Some last
            build (node :: acc) tail
        | [] ->
            let first = List.last acc
            let last = List.head acc
            first.Last := Some last
            last.Next := Some first
            acc

    let first = numbers.[0]

    build [ (makeNode first) ] (numbers.[1..] @ [ 10 .. 1000000 ])
    |> List.map (fun node -> node.Value, node)
    |> Map.ofList,
    first

let rec iter n (node: Node) =
    if n = 0
    then []
    else node.Value :: (iter (n - 1) (mustNextCup node))

let rec isIn v n (node: Node) =
    if n = 0 then false
    else if node.Value = v then true
    else isIn v (n - 1) (mustNextCup node)

let move current (deck: Map<_, _>) =
    let currentCup = deck.[current]
    let nextCup = mustNextCup currentCup
    let endOf3 = nextCup |> mustNextCup |> mustNextCup
    let nextCurrent = endOf3 |> mustNextCup

    let rec selectFrom number =
        if number = 0 then
            selectFrom 1_000_000
        else
            match isIn number 3 nextCup with
            | true -> selectFrom (number - 1)
            | false -> number

    let target = selectFrom (currentCup.Value - 1)

    let insertAfter = deck.[target]
    let insertBefore = mustNextCup insertAfter

    // move the 3 next cups behind the target cup
    insertAfter.Next := Some nextCup
    nextCup.Last := Some insertAfter
    insertBefore.Last := Some endOf3
    endOf3.Next := Some insertBefore

    // update the pointers from the current and
    // the next cup
    currentCup.Next := Some nextCurrent
    nextCurrent.Last := Some currentCup

    nextCurrent.Value

let moves n (s: string) =
    let deck, start = deckFrom s

    let rec loop acc current =
        if acc > 0 && acc % 100000 = 0 then printfn "%d moves left" acc

        if acc > 0 then
            let current' = move current deck
            loop (acc - 1) current'
        else
            ()

    printfn "Deck initialised; start playing"
    loop n start
    let one = deck.[1]
    iter 2 (mustNextCup one)

// Testdata
let testResult = moves 10_000_000 "389125467"

// Part 2
let result = moves 10_000_000 "974618352"

(uint64 result.[0]) * (uint64 result.[1])

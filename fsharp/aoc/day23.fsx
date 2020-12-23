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

    build [ (makeNode first) ] numbers.[1..]
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
        if number = 1
        then if isIn 9 3 nextCup then selectFrom 9 else 9
        else if isIn (number - 1) 3 nextCup
        then selectFrom (number - 1)
        else number - 1

    let target = selectFrom currentCup.Value

    let insertAfter = deck.[target]
    let insertBefore = mustNextCup insertAfter

    // move the pointers:
    insertAfter.Next := Some nextCup
    nextCup.Last := Some insertAfter

    insertBefore.Last := Some endOf3
    endOf3.Next := Some insertBefore

    currentCup.Next := Some nextCurrent
    nextCurrent.Last := Some currentCup

    nextCurrent.Value

let moves n (s: string) =
    let deck, start = deckFrom s

    let rec loop acc current =
        if acc > 0 then
            let current' = move current deck
            loop (acc - 1) current'
        else
            ()

    loop n start
    let one = deck.[1]
    iter 8 (mustNextCup one)

// Testdata
moves 100 "389125467"
|> List.map string
|> System.String.Concat

// Part 1
moves 100 "974618352"
|> List.map string
|> System.String.Concat

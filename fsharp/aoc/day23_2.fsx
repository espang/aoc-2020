open System.Collections.Generic

let deckFrom (s: string) =
    let numbers =
        s |> Seq.map (fun c -> int c - 48) |> Seq.toList

    LinkedList (numbers@[10..1_000_000])

let next (node:LinkedListNode<_>) (ll:LinkedList<_>) =
    if isNull node.Next
    then ll.First
    else node.Next

let move (currentCup:LinkedListNode<_>) (ll:LinkedList<int>) (deck: Dictionary<_, LinkedListNode<_>>) =
    let n1 = next currentCup ll
    let n2 = next n1 ll
    let n3 = next n2 ll

    ll.Remove(n1)
    ll.Remove(n2)
    ll.Remove(n3)
    
    let s = Set.empty.Add(n1.Value).Add(n2.Value).Add(n3.Value)
    
    let rec selectFrom number =
        if number = 0 then
            selectFrom 1_000_000
        else
            if s.Contains number
            then
                selectFrom (number - 1)
            else
                number
    
    let target = selectFrom (currentCup.Value - 1)
    ll.AddAfter(deck.[target], n1)
    ll.AddAfter(n1, n2)
    ll.AddAfter(n2, n3)

    next currentCup ll

let rec iter n (node:LinkedListNode<_>) =
    if n = 0
    then []
    else node.Value :: (iter (n - 1) node.Next)

let moves n (s: string) =
    printfn "make deck"
    let deck = deckFrom s
    let lookup = Dictionary()
    printfn "make lookup"
    let rec makeMap (ll:LinkedListNode<_>) =
        if isNull ll
        then ()
        else 
            lookup.Add(ll.Value, ll)
            makeMap ll.Next
    makeMap deck.First
  
    let rec loop acc current =
        if acc > 0 && acc % 100000 = 0 then printfn "%d moves left" acc

        if acc > 0 then
            let current' = move current deck lookup
            loop (acc - 1) current'
        else
            ()

    printfn "Deck initialised; start playing"
    loop n deck.First
    let one = deck.Find(1)
    iter 2 one.Next

// Testdata
#time
let testResult = moves 10_000_000 "389125467"
#time

// Part 2
#time
let result = moves 10_000_000 "974618352"
#time
(uint64 result.[0]) * (uint64 result.[1])


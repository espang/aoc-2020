open System.IO

type Instruction =
    | Acc of int
    | Jmp of int
    | Nop of int

let instructionFrom (s: string) =
    let items = s.Split()
    match items.[0] with
    | "acc" -> Some(Acc(int items.[1]))
    | "jmp" -> Some(Jmp(int items.[1]))
    | "nop" -> Some(Nop(int items.[1]))
    | _ -> None

let execute (program: Instruction []) =
    let rec e (acc, seen) index =
        if index >= program.Length then
            if index > program.Length
            then failwith "program didn't end directly after the last instruction"
            else (acc, true)
        else if Set.contains index seen then
            (acc, false)
        else
            match program.[index] with
            | Acc v -> e (acc + v, Set.add index seen) (index + 1)
            | Jmp v -> e (acc, Set.add index seen) (index + v)
            | Nop _ -> e (acc, Set.add index seen) (index + 1)

    e (0, Set.empty) 0

let program =
    File.ReadAllLines "input_8.txt"
    |> Seq.map instructionFrom
    |> Seq.choose id
    |> Seq.toArray

// part 1
execute program

let copyWith index newInstruction (program: Instruction []) =
    Array.init program.Length (fun x -> if x = index then newInstruction else program.[x])

let findFinishingProgram (program: Instruction []) =
    let rec find index =
        match program.[index] with
        | Acc _ -> find (index + 1)
        | Jmp v ->
            match execute (copyWith index (Nop v) program) with
            | (v, true) -> v
            | _ -> find (index + 1)
        | Nop v ->
            match execute (copyWith index (Jmp v) program) with
            | (v, true) -> v
            | _ -> find (index + 1)

    find 0

// part 2
findFinishingProgram program

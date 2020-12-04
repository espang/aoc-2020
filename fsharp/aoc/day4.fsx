open System.IO

let parsePassport (r: string) =
    r.Split()
    |> Array.filter (fun x -> x <> "")
    |> Array.map (fun x ->
        let pair = x.Split(":")
        pair.[0], pair.[1])
    |> dict

let passports =
    (File.ReadAllText "input_4.txt").Split("\n\n")
    |> Array.map parsePassport

let requiredKeys =
    [ "byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid" ]

let passportValid (p: System.Collections.Generic.IDictionary<string, string>) =
    let rec _check fields =
        match fields with
        | h :: t -> if not (p.ContainsKey h) then false else _check t
        | [] -> true

    _check requiredKeys

let p1ValidPassports = Array.filter passportValid passports
p1ValidPassports.Length

let checkTextIsNumberBetween lower upper s =
    try
        let number = int ""
        (number >= lower) && (number <= upper)
    with :? System.FormatException -> false

let checkHeight (x: string) =
    if x.EndsWith "cm"
    then checkTextIsNumberBetween 150 193 x.[..(x.Length - 3)]
    elif x.EndsWith "in"
    then checkTextIsNumberBetween 59 76 x.[..(x.Length - 3)]
    else false

let checkColor (x: string) =
    (x.StartsWith "#")
    && (Seq.forall System.Char.IsDigit x.[1..])

let checkEyeColor x =
    match x with
    | "amb"
    | "blu"
    | "brn"
    | "gry"
    | "grn"
    | "hzl"
    | "oth" -> true
    | _ -> false

let checkPassportId (x: string) =
    x.Length = 9 && (Seq.forall System.Char.IsDigit x)

let passportValid2 (p: System.Collections.Generic.IDictionary<string, string>) =
    let rec _check fields =
        match fields with
        | h :: t ->
            if not (p.ContainsKey h) then
                false
            elif match h with
                    | "byr" -> checkTextIsNumberBetween 1920 2002 (p.Item h)
                    | "iyr" -> checkTextIsNumberBetween 2010 2020 (p.Item h)
                    | "eyr" -> checkTextIsNumberBetween 2020 2030 (p.Item h)
                    | "hgt" -> checkHeight (p.Item h)
                    | "hcl" -> checkColor (p.Item h)
                    | "pid" -> checkPassportId (p.Item h)
                    | _ -> false
            then _check t
            else
                false
        | [] -> true

    _check requiredKeys

let p2ValidPassports = Array.filter passportValid2 passports
p2ValidPassports.Length

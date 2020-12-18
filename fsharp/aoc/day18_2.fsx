open System
open System.IO

type Token = Number of int | Mul | Add | Open | Close

type Expr =
    | Addition of Expr * Expr
    | Multiplication of Expr * Expr
    | Value of int

type TokenOrExpr = 
    | T of Token
    | E of Expr

let charToInt c = (int c) - 48

let lex (line: char list) = 
    let rec _lex acc line =
        match line with
        | '(' :: tail ->
            _lex (Open::acc) tail
        | ')' :: tail -> _lex (Close::acc) tail
        | '*' :: tail -> _lex (Mul::acc) tail
        | '+' :: tail -> _lex (Add::acc) tail
        | d :: tail when Char.IsDigit d -> 
            _lex ((Number (charToInt d))::acc) tail
        | ' ' :: tail -> _lex acc tail
        | [] -> List.rev acc
        | c :: _ -> failwithf "unexpected char %A" c

    _lex [] line

let rec parse tokens : Expr * TokenOrExpr list=
    let rec _handleParenthesis (acc:TokenOrExpr list) (ts: TokenOrExpr list) =
        match ts with
        | (T t)::tail ->
            match t with
            | Open ->
                let (expression, rest) = parse tail
                _handleParenthesis ((E expression)::acc) rest
            | Close ->
                acc, tail
            | other -> _handleParenthesis ((T other)::acc) tail
        | [] -> List.rev acc, []
        | _ -> failwith "unexpected expression"

    let withoutParens, tail =
        _handleParenthesis [] tokens

    let rec _handlePlus (acc:Expr list) (last:Expr) (ts:TokenOrExpr list) =
        match ts with
        | (T Add)::next::tail ->
            let nextE = 
                match next with
                | E e -> e
                | T t ->
                    match t with
                    | Number n -> Value n
                    | _ -> failwith "expect token to be a number!"
            let e = Addition (last, nextE)
            _handlePlus acc e tail
        | (T Mul)::tail ->
            _handlePlus (last::acc) last tail
        | (T (Number n))::tail ->
            _handlePlus acc (Value n) tail
        | (E e)::tail ->
            _handlePlus acc e tail
        | [] -> List.rev (last::acc)
        | x::tail -> failwithf "unexpected token or expr %A" x
    
    let expressions =_handlePlus [] (Value 0) withoutParens
    
    List.fold (fun e lhs -> Multiplication(e, lhs)) expressions.[0] expressions.[1..], tail

let rec eval expression =
    match expression with
    | Multiplication (lhs, rhs)->
        eval lhs * eval rhs
    | Addition (lhs, rhs)->
        eval lhs + eval rhs
    | Value v->
        uint64 v

// part 2
File.ReadAllLines "input_18.txt"
|> Seq.filter (fun line -> line <> "")
|> Seq.sumBy (Seq.toList >> lex >> List.map (T) >> parse >> fst >> eval)

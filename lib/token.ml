type t =
  | Digit of int
  | Ident of string
  | LeftChevron
  | RightChevron
  | LeftParen
  | RightParen
  | Equal
  | Plus
  | Minus
  | Comma
  | If
  | Else
  | Then
  | Let
  | Or
  | And
  | True
  | False
  | EOF

type state = t * char Seq.t

let eq x y =
  match (x, y) with
  | Digit i1, Digit i2 -> i1 = i2
  | Ident s1, Ident s2 -> s1 = s2
  | x, y -> x = y

let pp ppf =
  Fmt.(
    function
    | Ident s -> pf ppf "Ident(%s)" s
    | Then -> pf ppf "then"
    | Else -> pf ppf "else"
    | Digit i -> pf ppf "Digit(%d)" i
    | LeftChevron -> pf ppf "<"
    | RightChevron -> pf ppf ">"
    | LeftParen -> pf ppf "("
    | RightParen -> pf ppf ")"
    | Plus -> pf ppf "+"
    | Minus -> pf ppf "-"
    | Comma -> pf ppf ","
    | Equal -> pf ppf "="
    | If -> pf ppf "if"
    | Let -> pf ppf "let"
    | Or -> pf ppf "or"
    | And -> pf ppf "and"
    | True -> pf ppf "true"
    | False -> pf ppf "false"
    | EOF -> pf ppf "EOF")

let keyword s =
  let module KM = Map.Make (String) in
  let keywords =
    KM.of_list
      [
        ("true", True);
        ("false", False);
        ("if", If);
        ("else", Else);
        ("then", Then);
        ("let", Let);
      ]
  in
  KM.find_opt s keywords

let digit i = Digit i
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_alphanumeric c = is_alpha c || is_digit c

let parse_identifier buf =
  let seq = Seq.take_while is_alphanumeric buf in
  let str = String.of_seq seq in
  match keyword str with 
  |Some kw -> kw, seq 
  |None -> Ident str, seq

let parse_int buf =
  let ds = Seq.take_while is_digit buf in
  let digit = digit @@ int_of_string @@ String.of_seq ds in
  digit, ds


let or_ f g = match f with Some _ -> f | None -> g
let choice l = List.fold_left or_ None l

[@@@ocaml.warning "-32-69-38-37"]

exception LexerError

type buffer = char Seq.t

type token =
  | Digit of int
  | Ident of string
  | LeftChevron (* < *)
  | RightChevron (* > *)
  | Equal (* = *)
  | True
  | False
  | If
  | Else
  | Then
  | Let
  | Or
  | And

let eq_token x y =
  match (x, y) with
  | Digit i1, Digit i2 -> i1 = i2
  | Ident s1, Ident s2 -> s1 = s2
  | x, y -> x = y

let pp_token ppf =
  Fmt.(
    function
    | Ident s -> pf ppf "Ident(%s)" s
    | Then -> pf ppf "then"
    | Else -> pf ppf "else"
    | Digit i -> pf ppf "Digit(%d)" i
    | LeftChevron -> pf ppf "<"
    | RightChevron -> pf ppf ">"
    | Equal -> pf ppf "="
    | True -> pf ppf "true"
    | False -> pf ppf "false"
    | If -> pf ppf "if"
    | Let -> pf ppf "let"
    | Or -> pf ppf "or"
    | And -> pf ppf "and")

let digit i = Digit i
let ident s = Ident s

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

type t = {
  input : string;
  curr : char Seq.t;
  mutable index : int;
  tokens : string list;
}

let default_lexer =
  { input = ""; curr = String.to_seq ""; index = 0; tokens = [] }

let of_string s = { default_lexer with input = s; curr = String.to_seq s }
let ok = Result.ok
let err = Result.error
let str = Fmt.str
let peek lexer = Seq.cons lexer.curr
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_alphanumeric c = is_alpha c || is_digit c

let parse_identifier buf =
  let ( let* ) = Option.bind in
  let* c, _ = Seq.uncons buf in
  if is_alpha c then
    let identifier = Seq.take_while is_alphanumeric buf in
    let str = String.of_seq identifier in
    Option.some
    @@ match keyword str with Some token -> token | None -> Ident str
  else None

let parse_int buf =
  let ds = Seq.take_while is_digit buf in
  if Seq.is_empty ds then None
  else Option.some @@ digit @@ int_of_string @@ String.of_seq ds

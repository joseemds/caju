[@@@ocaml.warning "-32-69-38-37"]

type stream = char Seq.t
type t = { buf : Buffer.t; curr : stream; index : int; tokens : Token.t list }

let default_lexer =
  { buf = Buffer.create 80; curr = String.to_seq ""; index = 0; tokens = [] }

let of_string s = { default_lexer with curr = String.to_seq s }
let peek lexer = Option.map fst @@ Seq.uncons lexer.curr
let is_digit = function '0' .. '9' -> true | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let consume lexer =
  match Seq.uncons lexer.curr with
  | Some (c, curr) ->
      Buffer.add_char lexer.buf c;
      { lexer with curr }
  | None -> lexer

let add_token lexer token = { lexer with tokens = token :: lexer.tokens }

let advance lexer =
  let open Token in
  let token = match Seq.uncons lexer.curr with
  | Some (c, _) -> (
      match c with
      | '<' -> LeftChevron
      | '>' -> RightChevron
      | '=' -> Equal
      | c when is_alpha c -> Token.parse_identifier lexer.curr
      | c when is_digit c -> Token.parse_int lexer.curr
      | _ -> Let)
  | None -> failwith "Expected Token" in
  add_token lexer token

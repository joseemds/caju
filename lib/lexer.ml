[@@@ocaml.warning "-32-69-38-37"]

type stream = char Seq.t
type t = { buf : Buffer.t; curr : stream; index : int; tokens : Token.t list }

let default_lexer =
  { buf = Buffer.create 80; curr = String.to_seq ""; index = 0; tokens = [] }

let of_string s = { default_lexer with curr = String.to_seq s }
let tokens lexer = List.rev lexer.tokens
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

let update_lexer lexer (token, buf) =
  { lexer with tokens = token :: lexer.tokens; curr = buf }

let advance lexer =
  let update = update_lexer lexer in
  let open Token in
  match Seq.uncons lexer.curr with
  | Some (c, buf) -> (
      match c with
      | ' ' -> lexer
      | '<' -> update (LeftChevron, buf)
      | '>' -> update (RightChevron, buf)
      | '=' -> update (Equal, buf)
      | '(' -> update (LeftParen, buf)
      | ')' -> update (RightParen, buf)
      | '+' -> update (Plus, buf)
      | '-' -> update (Minus, buf)
      | c when is_digit c -> update @@ Token.parse_int lexer.curr
      | c when is_alpha c -> update @@ Token.parse_identifier lexer.curr
      | _ -> Fmt.epr "here @?"; update (EOF, Seq.empty))
  | None -> update (EOF, Seq.empty)

let rec tokenize lexer =
  let lexer' = advance lexer in
  if lexer'.curr = Seq.empty then
    tokens lexer'
  else
    tokenize lexer

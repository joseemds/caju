[@@@ocaml.warning "-32-69-38-37"]

type stream = char Seq.t
type t = { buf : Buffer.t; curr : stream; index : int; tokens : string list }

let default_lexer =
  { buf = Buffer.create 80; curr = String.to_seq ""; index = 0; tokens = [] }

let of_string s = { default_lexer with curr = String.to_seq s }
let peek lexer = Option.map fst @@ Seq.uncons lexer.curr

let consume lexer =
  match Seq.uncons lexer.curr with
  | Some (c, curr) ->
      Buffer.add_char lexer.buf c;
      { lexer with curr }
  | None -> lexer

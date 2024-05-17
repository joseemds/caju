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

type t

type buffer = char Seq.t

val pp_token :token Fmt.t
val eq_token : token -> token -> bool
val parse_int : buffer -> token option
val parse_identifier : buffer -> token option

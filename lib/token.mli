type t =
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

val pp : t Fmt.t
val eq : t -> t -> bool
val parse_int : Lexer.stream -> t option
val parse_identifier : Lexer.stream -> t option

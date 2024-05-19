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

val pp : t Fmt.t
val eq : t -> t -> bool
val parse_int : char Seq.t -> t
val parse_identifier : char Seq.t -> t
val choice : t option list -> t option

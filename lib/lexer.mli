type stream = char Seq.t
type t

val add_token : t -> Token.t -> t

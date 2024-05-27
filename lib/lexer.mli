type stream = char Seq.t
type t

val add_token : t -> Token.t -> t
val tokens : t -> Token.t list
val consume: t -> t
val advance: t -> t
val tokenize: t -> Token.t list
val of_string: string -> t

type tag = { name : string; attributes : (string * string option) list; } [@@deriving show]
type element = [ `node of node | `text of string ] [@@deriving show]
and node = { tag : tag; children : element list; } [@@deriving show]
val parse : ?allow_unquoted:bool -> ?allow_unclosed:bool -> string -> node

type selector = [
  | `attr of string * string option
  | `cla of string
  | `id of string
  | `tag of string
]
val find : node -> selector list -> node option
val get : node -> selector list -> node
val list : node -> selector list -> node list
val texts : node -> string list
val text : ?sep:string -> node -> string

val to_string : ?indent:int -> node -> string

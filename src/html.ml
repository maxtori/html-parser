type tag = {
  name: string;
  attributes: (string * string option) list;
} [@@deriving show]

type token = [
  | `text of string
  | `tag of tag
  | `close of string
  | `comment of string
  | `doctype
]

type 'a b = {
  o: int;
  s: string;
  r: 'a;
}

type element = [
  | `node of node
  | `text of string
] [@@deriving show]

and node = {
  tag: tag;
  children: element list
} [@@deriving show]

let empty_chars = [ ' '; '\n'; '\t' ]

let rec next_char l s i =
  try
    let c = String.get s i in
    if List.mem c l then Some (i, c)
    else next_char l s (i+1)
  with _ -> None

let get_text ({o; s; _} as b) : token b =
  match String.index_from_opt s o '<' with
  | None -> failwith "failed to parse text"
  | Some j -> { b with o=j; r=`text (String.sub s o (j-o)) }

let get_close ({o; s; _} as b) : token b =
  match String.index_from_opt s o '>' with
  | None -> failwith "failed to parse closing tag"
  | Some j -> { b with o=j+1; r=`close (String.sub s (o+2) (j-o-2)) }

let get_attribute ({o; s; _ } as b) =
  match next_char ('=' :: '>' :: empty_chars) s o with
  | None -> failwith "failed to parse attribute"
  | Some (j, '>') -> {b with o=j; r = (String.sub s o (j-o-1), None)}
  | Some (j, '=') ->
    let quote = String.get s (j+1) in
    begin match String.index_from_opt s (j+2) quote with
      | None -> failwith "failed to parse attribute value"
      | Some k ->
        {b with o=k+1; r = (String.sub s o (j-o), Some (String.sub s (j+2) (k-j-2)))}
    end
  | Some (j, _) ->
    {b with o=j; r=(String.sub s o (j-o), None)}

let rec get_attributes ({o; s; r} as b) =
  if o = String.length s then b
  else
    let c = String.get s o in
    if List.mem c empty_chars then get_attributes {o=o+1; s; r}
    else if c = '>' then {b with o=o+1}
    else
      let a = get_attribute {o; s; r=()} in
      get_attributes {b with o=a.o; r = r @ [ a.r ]}

let get_tag {o; s; _} : token b =
  match next_char ('>' :: empty_chars) s o with
  | None -> failwith "failed to parse tag"
  | Some (j, '>') ->
    let name = String.sub s (o+1) (j-o-1) in
    { o=j+1; s; r=`tag {name; attributes=[]} }
  | Some (j, _) ->
    let name = String.sub s (o+1) (j-o-1) in
    let a = get_attributes {o=j; s; r=[]} in
    { o=a.o; s; r=`tag {name; attributes=a.r} }

let get_comment {o; s; _} : token b =
  match String.index_from_opt s o '>' with
  | None -> failwith "failed to parse comment"
  | Some j ->
    let c = String.sub s (o+2) (j-o-2) in
    match String.split_on_char ' ' c with
    | "doctype" :: _ | "DOCTYPE" :: _ -> {s; o=j+1; r=`doctype}
    | _ ->
      let c = String.trim @@ String.sub c 2 (String.length c - 4) in
      {s; o=j+1; r=`comment c}

let get_token b : token b =
  if String.get b.s b.o <> '<' then get_text b
  else
    let c = String.get b.s (b.o+1) in
    if c = '/' then get_close b
    else if c = '!' then get_comment b
    else get_tag b

let rec get_tokens b =
  if String.length b.s = b.o then
    List.fold_left (fun acc t -> match t with
        | `text s ->
          let s = String.trim s in
          if s = "" then acc else `text s :: acc
        | t -> t :: acc) [] b.r
  else
    let el = get_token b in
    get_tokens { el with r = el.r :: b.r }

let self_closing = [
  "area"; "base"; "br"; "col"; "embed"; "hr"; "img"; "input"; "link"; "meta";
  "param"; "source"; "track"; "vbr"
]

let elements_of_tokens (l: token list) =
  let rec aux tag children = function
    | `tag t :: q ->
      let child, q =
        if List.mem t.name self_closing then `node { tag = t; children=[] }, q
        else aux t [] q in
      aux tag (child :: children) q
    | `text s :: q -> aux tag (`text s :: children) q
    | (`comment _ | `doctype) :: q -> aux tag children q
    | `close name :: q when name = tag.name ->
      `node { tag; children=List.rev children }, q
    | _l -> failwith "wrong html structure 0" in
  match l with
  | `doctype :: `tag t :: q
  | `tag t :: q ->
    begin match aux t [] q with
      | `node node, [] -> node
      | _ -> failwith "wrong html structure 1"
    end
  | _ -> failwith "wrong html structure 2"

let parse s =
  let tokens = get_tokens { s=String.trim s; o=0; r=[] } in
  elements_of_tokens tokens

type selector = [
  | `id of string
  | `cla of string
  | `tag of string
  | `attr of (string * string option)
]

let select t s =
  List.for_all (function
    | `id n -> List.exists (function ("id", Some v) when n = v -> true | _ -> false) t.attributes
    | `tag n -> t.name = n
    | `attr (k, v) -> List.exists (function (k2, v2) when k = k2 && v = v2 -> true | _ -> false) t.attributes
    | `cla c ->
      List.exists (function ("class", Some v) -> List.mem c (String.split_on_char ' ' v) | _ -> false) t.attributes
  ) s

let find n s =
  let rec aux = function
    | `node { tag; children } :: q ->
      if select tag s then Some {tag; children}
      else aux (q @ children)
    | _ :: q -> aux q
    | [] -> None in
  aux n.children

let get n s = Option.get (find n s)

let list n s =
  let rec aux acc = function
    | `node { tag; children } :: q ->
      if select tag s then aux ({tag; children} :: acc) q
      else aux acc (q @ children)
    | _ :: q -> aux acc q
    | [] -> List.rev acc in
  aux [] n.children

let texts n =
  let rec aux acc = function
    | `node { children; _ } :: q ->
      let acc2 = aux [] children in
      aux (acc @ acc2) q
    | `text s :: q -> aux (acc @ [ s ]) q
    | [] -> acc in
  aux [] n.children

let text ?(sep="\n") n = String.concat sep (texts n)

let to_string ?(indent=2) n =
  let rec aux off x =
    let offset = String.make (indent*off) ' ' in
    match x with
    | `text t -> offset ^ t
    | `node n ->
      let attribute (k, v) = match v with
        | None -> k
        | Some v -> Format.sprintf "%s=%S" k v in
      let attributes = match n.tag.attributes with
        | [] -> ""
        | l -> " " ^ String.concat " " @@ List.map attribute l in
      if List.mem n.tag.name self_closing then
        Format.sprintf "%s<%s%s/>" offset n.tag.name attributes
      else
        let children = match n.children with
          | [] -> offset
          | l -> Format.sprintf "\n%s\n%s" (String.concat "\n" @@ List.map (aux (off+1)) l) offset in
        Format.sprintf "%s<%s%s>%s</%s>" offset n.tag.name attributes children n.tag.name in
  aux 0 (`node n)

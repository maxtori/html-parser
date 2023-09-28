open Html

let () =
  let ic = try open_in Sys.argv.(1) with _ -> failwith "cannot read file" in
  let s = really_input_string ic (in_channel_length ic) in
  let node = parse ~allow_unquoted:true ~allow_unclosed:true s in
  Format.printf "%a@." pp_node node;
  Format.printf "[%s]@." @@ String.concat "; " @@
  List.map (fun l -> Format.sprintf "[%s]" @@ String.concat "; " l) @@
  (List.map texts (list node [`tag "div"]));
  let html = to_string node in
  Format.printf "%s@." html

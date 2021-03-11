open Ppxlib

let string_constants_of =
  object
    inherit [string list] Ast_traverse.fold as super

    method! expression e acc =
      let acc = super#expression e acc in
      match e.pexp_desc with
      | Pexp_constant (Pconst_string (s, _)) ->
          ("Pexp_constant ( " ^ "Pconst_string (\"" ^ s ^ "\"))") :: acc
      | Pexp_constant (Pconst_integer (i, _)) ->
          ("Pexp_constant (" ^ "Pconst_string (\"" ^ i ^ "\"))") :: acc
      | Pexp_ident { txt = Lident op; _ } ->
          ("Pexp_ident " ^ "{txt = Lident \"" ^ op ^ "\"} ") :: acc
      | _ -> acc

    method! pattern p acc =
      let acc = super#pattern p acc in
      match p.ppat_desc with
      | Ppat_constant (Pconst_string (s, _)) -> s :: acc
      | _ -> acc
  end

let string_constants_of_expression = string_constants_of#expression

let rec ident l ~i =
  match l with
  | [] -> []
  | e :: t ->
      if String.sub e 0 10 |> String.equal "Pexp_ident" then
        (i ^ e) :: ident t ~i:("----" ^ i)
      else (i ^ e) :: ident t ~i

let transform source =
  try
  let v = Parse.expression (Lexing.from_string source) in
  (* print_int (List.length (string_constants_of_expression v [])) *)
  let ast_list =
    string_constants_of_expression v [] |> List.rev |> ident ~i:""
  in
  List.fold_left (fun x y -> x ^ "\n" ^ y) "" ast_list
with _->"Syntax error"

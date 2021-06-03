open Ppxlib
open Jsonoo.Encode

let parse_ast =
  object (self)
    inherit [Jsonoo.t] Traverse_ast.lift as super

    method unit () = null

    method tuple args = object_ (("type", string "tuple") :: args)

    method string value = string value

    method float value = float value

    method bool value = bool value

    method record label args = object_ (("type", string label) :: args)

    method other _ = string "serializing other values is not supported yet"

    method nativeint _ = string "serializing nativeint is not supported yet"

    method int64 _ = string "serializing int64 is not supported yet"

    method int32 _ = string "serializing int32 is not supported yet"

    method int value = int value

    method list f = list f

    method option f o =
      match o with
      | Some x ->
        let a = f x in
        a
      | None -> Jsonoo.Encode.object_ [ ("type", Jsonoo.Encode.string "None") ]

    method constr label args =
      match args with
      | [] -> object_ [ ("type", string label) ]
      | _ -> object_ (("type", string label) :: args)

    method char value = char value

    method array f arg = list id (Array.to_list arg |> List.map f)

    method! structure_item { pstr_desc; pstr_loc } =
      object_
        [ ("type", string "structure_item")
        ; ("pstr_desc", super#structure_item_desc pstr_desc)
        ; ("pstr_loc", super#location pstr_loc)
        ]

    (* method! expression_desc = function | Pexp_constraint ({ pexp_desc; _ },
       _) -> super#expression_desc pexp_desc | arg -> super#expression_desc arg *)

    method! open_infos _a { popen_expr; popen_override; popen_loc; _ } =
      let popen_expr = _a popen_expr in
      let popen_override = self#override_flag popen_override in
      let popen_loc = self#location popen_loc in
      let popen_attributes = null in
      self#record "open_infos"
        [ ("popen_expr", popen_expr)
        ; ("popen_override", popen_override)
        ; ("popen_loc", popen_loc)
        ; ("popen_attributes", popen_attributes)
        ]

    method! structure s = object_ [ ("structure", list self#structure_item s) ]

    (* method! class_infos : 'a . ('a -> Jsonoo.t) -> 'a class_infos ->
       Jsonoo.t= fun _a -> fun { pci_virt; pci_params; pci_name; pci_expr;
       pci_loc; pci_attributes } -> let pci_virt = self#virtual_flag pci_virt in
       let pci_params = self#list (fun (a, b) -> let a = self#core_type a in let
       b = (fun (a, b) -> let a = self#variance a in let b = self#injectivity b
       in tuple2 id id (a,b)) b in tuple2 id id (a,b)) pci_params in let
       pci_name = self#loc self#string pci_name in let pci_expr = _a pci_expr in
       let pci_loc = self#location pci_loc in let pci_attributes =
       self#attributes pci_attributes in self#record "class_infos" [("pci_virt",
       pci_virt); ("pci_params", pci_params); ("pci_name", pci_name);
       ("pci_expr", pci_expr); ("pci_loc", pci_loc); ("pci_attributes",
       pci_attributes)] *)
  end

let transform source =
  try
    let v = Parse.implementation (Lexing.from_string source) in
    parse_ast#structure v
  with
  | _ -> Jsonoo.Encode.string "Syntax error"

let from_structure (structure : Parsetree.structure) =
  try parse_ast#structure structure with
  | _ -> Jsonoo.Encode.string "Syntax error"

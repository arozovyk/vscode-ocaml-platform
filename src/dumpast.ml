open Ppxlib

(* let _ = raise (Traverse_ast2.Reparse_error ""); *)
let reparse_ast =
  object (self)
    inherit [Jsonoo.t] Traverse_ast2.lift2 as super

    method unit () = Jsonoo.Encode.null

    method tuple args =
      Jsonoo.Encode.object_ (("type", Jsonoo.Encode.string "tuple") :: args)

    method string value _ = Jsonoo.Encode.string value

    method float value _ = Jsonoo.Encode.float value

    method bool value _ = Jsonoo.Encode.bool value

    method record label args =
      Jsonoo.Encode.object_ (("type", Jsonoo.Encode.string label) :: args)

    method other _ _ =
      Jsonoo.Encode.string "serializing other values is not supported yet"

    method nativeint _ _ =
      Jsonoo.Encode.string "serializing nativeint is not supported yet"

    method int64 _ _ =
      Jsonoo.Encode.string "serializing int64 is not supported yet"

    method int32 _ _ =
      Jsonoo.Encode.string "serializing int32 is not supported yet"

    method int value _ = Jsonoo.Encode.int value

    method list f l l' =
      (*FIXME different length?*)
      List.map2 f l l' |> Jsonoo.Encode.list (fun x -> x)

    method option f o o' =
      match (o, o') with
      | Some x, Some x' ->
        let a = f x x' in
        a
      | None, None ->
        Jsonoo.Encode.object_ [ ("type", Jsonoo.Encode.string "None") ]
      | _ -> raise (Traverse_ast2.Reparse_error "option")

    method constr label args =
      match args with
      | [] -> Jsonoo.Encode.object_ [ ("type", Jsonoo.Encode.string label) ]
      | _ ->
        Jsonoo.Encode.object_ (("type", Jsonoo.Encode.string label) :: args)

    method char value _ = Jsonoo.Encode.char value

    method! location : location -> location -> Jsonoo.t =
      fun { loc_start; loc_end; loc_ghost }
          { loc_start = loc_start'; loc_end = loc_end'; loc_ghost = loc_ghost' } ->
        (* print_endline (" locs are " ^ Int.to_string loc_start.pos_cnum^" " ^
           Int.to_string loc_start'.pos_cnum); *)
        let loc_start = super#position loc_start loc_start' in
        let loc_end = super#position loc_end loc_end' in
        let loc_ghost = self#bool loc_ghost loc_ghost' in
        self#record "Location.t"
          [ ("loc_start", loc_start)
          ; ("loc_end", loc_end)
          ; ("loc_ghost", loc_ghost)
          ]
    (* method array f arg = list id (Array.to_list arg |> List.map f) *)
    (* method! structure_item { pstr_desc; pstr_loc } = object_ [ ("type",
       string "structure_item") ; ("pstr_desc", super#structure_item_desc
       pstr_desc) ; ("pstr_loc", super#location pstr_loc) ] *)

    (* method! expression_desc = function | Pexp_constraint ({ pexp_desc; _ },
       _) -> super#expression_desc pexp_desc | arg -> super#expression_desc arg *)

    (* method! structure s s' = Jsonoo.Encode.object_ [ ( "structure" ,
       List.map2 self#structure_item s s' |> Jsonoo.Encode.list (fun x -> x) ) ] *)

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

let reparse s s' = reparse_ast#structure s s'

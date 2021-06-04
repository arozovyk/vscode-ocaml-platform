open Ast

type diff =
  { method_name : string
  ; fst_node_pp : string
  ; snd_node_pp : string
  }
[@@deriving ord]

let diff_to_string { method_name; fst_node_pp; snd_node_pp } =
  "method_name: " ^ method_name ^ "\n\n" ^ "fst_node_pp:\n\n" ^ fst_node_pp
  ^ "\n\n" ^ "snd_node_pp:\n\n" ^ snd_node_pp

let i = ref 0

let add_diff method_name fst_node_pp snd_node_pp acc =
  let r = { method_name; fst_node_pp; snd_node_pp } in
  i := !i + 1;
  r :: acc

type json = Jsonoo.t

exception Reparsing_error

class lift3 =
  object (self)
    method record : (string * json) list -> json = fun _ -> Jsonoo.Encode.null

    method tuple : (string * json) list -> json = fun _ -> Jsonoo.Encode.null

    method constr : string -> json list -> json = fun _ _ -> Jsonoo.Encode.null

    method bool : bool -> bool -> json = fun _ b -> Jsonoo.Encode.bool b

    method char : char -> char -> json = fun _ c -> Jsonoo.Encode.char c

    method int : int -> int -> json = fun _ i -> Jsonoo.Encode.int i

    method list : 'a. ('a -> 'a -> json) -> 'a list -> 'a list -> json =
      fun f l l' -> Jsonoo.Encode.list (fun x -> x) (List.map2 f l l')

    method option : 'a. ('a -> 'a -> json) -> 'a option -> 'a option -> json =
      fun f o o' ->
        match (o, o') with
        | Some x, Some y ->
          let a = f x y in
          a
        | _ -> raise Reparsing_error

    method string : string -> string -> json = fun _ s -> Jsonoo.Encode.string s

    method position : position -> position -> json =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum }
          { pos_fname = pos_fname'
          ; pos_lnum = pos_lnum'
          ; pos_bol = pos_bol'
          ; pos_cnum = pos_cnum'
          } ->
        let pos_fname = self#string pos_fname pos_fname' in
        let pos_lnum = self#int pos_lnum pos_lnum' in
        let pos_bol = self#int pos_bol pos_bol' in
        let pos_cnum = self#int pos_cnum pos_cnum' in
        self#record
          [ ("type", Jsonoo.Encode.string "Lexing.position")
          ; ("pos_fname", pos_fname)
          ; ("pos_lnum", pos_lnum)
          ; ("pos_bol", pos_bol)
          ; ("pos_cnum", pos_cnum)
          ]

    method location : location -> location -> json =
      fun { loc_start; loc_end; loc_ghost }
          { loc_start = loc_start'; loc_end = loc_end'; loc_ghost = loc_ghost' } ->
        let loc_start = self#position loc_start loc_start' in
        let loc_end = self#position loc_end loc_end' in
        let loc_ghost = self#bool loc_ghost loc_ghost' in
        self#record
          [ ("type", Jsonoo.Encode.string "Location.t")
          ; ("loc_start", loc_start)
          ; ("loc_end", loc_end)
          ; ("loc_ghost", loc_ghost)
          ]

    method location_stack : location_stack -> location_stack -> json =
      fun l l' -> self#list self#location l l'

    method loc : 'a. ('a -> 'a -> json) -> 'a loc -> 'a loc -> json =
      fun _a { txt; loc } { txt = txt'; loc = loc' } ->
        let txt = _a txt txt' in
        let loc = self#location loc loc' in
        self#record
          [ ("type", Jsonoo.Encode.string "loc"); ("txt", txt); ("loc", loc) ]

    method longident : longident -> longident -> json =
      fun x x' ->
        match (x, x') with
        | Lident a, Lident a' ->
          let a = self#string a a' in
          self#constr "Lident" [ a ]
        | Ldot (a, b), Ldot (a', b') ->
          let a = self#longident a a' in
          let b = self#string b b' in
          self#constr "Ldot" [ a; b ]
        | Lapply (a, b), Lapply (a', b') ->
          let a = self#longident a a' in
          let b = self#longident b b' in
          self#constr "Lapply" [ a; b ]
        | _, _ -> raise Reparsing_error

    method longident_loc : longident_loc -> longident_loc -> json =
      fun l l' -> self#loc (self#longident l l')

    method rec_flag : rec_flag -> rec_flag -> json =
      fun x x' ->
        match (x, x') with
        | Nonrecursive, Nonrecursive -> self#constr "Nonrecursive" []
        | Recursive, Recursive -> self#constr "Recursive" []
        | _ -> raise Reparsing_error

    method direction_flag : direction_flag -> direction_flag -> json =
      fun x x' ->
        match (x, x') with
        | Upto, Upto -> self#constr "Upto" []
        | Downto, Downto -> self#constr "Downto" []
        | _ -> raise Reparsing_error

    method private_flag : private_flag -> private_flag -> json =
      fun x x' ->
        match (x, x') with
        | Private, Private -> self#constr "Private" []
        | Public, Public -> self#constr "Public" []
        | _ -> raise Reparsing_error

    method mutable_flag : mutable_flag -> mutable_flag -> json =
      fun x x' ->
        match (x, x') with
        | Immutable, Immutable -> self#constr "Immutable" []
        | Mutable, Mutable -> self#constr "Mutable" []
        | _ -> raise Reparsing_error

    method virtual_flag : virtual_flag -> virtual_flag -> json =
      fun x x' ->
        match (x, x') with
        | Virtual, Virtual -> self#constr "Virtual" []
        | Concrete, Concrete -> self#constr "Concrete" []
        | _ -> raise Reparsing_error

    method override_flag : override_flag -> override_flag -> json =
      fun x x' ->
        match (x, x') with
        | Override, Override -> self#constr "Override" []
        | Fresh, Fresh -> self#constr "Fresh" []
        | _ -> raise Reparsing_error

    method closed_flag : closed_flag -> closed_flag -> json =
      fun x x' ->
        match (x, x') with
        | Closed, Closed -> self#constr "Closed" []
        | Open, Open -> self#constr "Open" []
        | _ -> raise Reparsing_error

    method label : label -> label -> json = fun l l' -> self#string l l'

    method arg_label : arg_label -> arg_label -> json =
      fun x x' ->
        match (x, x') with
        | Nolabel, Nolabel -> self#constr "Nolabel" []
        | Labelled a, Labelled a' ->
          let a = self#string a a' in
          self#constr "Labelled" [ a ]
        | Optional a, Optional a' ->
          let a = self#string a a' in
          self#constr "Optional" [ a ]
        | _ -> raise Reparsing_error

    method variance : variance -> variance -> json =
      fun x x' ->
        match (x, x') with
        | Covariant, Covariant -> self#constr "Covariant" []
        | Contravariant, Contravariant -> self#constr "Contravariant" []
        | NoVariance, NoVariance -> self#constr "NoVariance" []
        | _ -> raise Reparsing_error

    method injectivity : injectivity -> injectivity -> json =
      fun x x' ->
        match (x, x') with
        | Injective, Injective -> self#constr "Injective" []
        | NoInjectivity, NoInjectivity -> self#constr "NoInjectivity" []
        | _ -> raise Reparsing_error

    method constant : constant -> constant -> json =
      fun x x' ->
        match (x, x') with
        | Pconst_integer (a, b), Pconst_integer (a', b') ->
          let a = self#string a a' in
          let b = self#option self#char b b' in
          self#constr "Pconst_integer" [ a; b ]
        | Pconst_char a, Pconst_char a' -> self#char a a'
        | Pconst_string (a, b, c), Pconst_string (a', b', c') ->
          let a = self#string a a' in
          let b = self#location b b' in
          let c = self#option self#string c c' in
          self#constr "Pconst_string" [ a; b; c ]
        | Pconst_float (a, b), Pconst_float (a', b') ->
          let a = self#string a a' in
          let b = self#option self#char b b' in
          self#constr "Pconst_float" [ a; b ]
        | _ -> raise Reparsing_error

    method attribute : attribute -> attribute -> json =
      fun { attr_name; attr_payload; attr_loc }
          { attr_name = attr_name'
          ; attr_payload = attr_payload'
          ; attr_loc = attr_loc'
          } ->
        let attr_name = self#loc self#string attr_name attr_name' in
        let attr_payload = self#payload attr_payload attr_payload' in
        let attr_loc = self#location attr_loc attr_loc' in
        Jsonoo.Encode.object_
          [ ("type", Jsonoo.Encode.string "attribute")
          ; ("loc_start", attr_name)
          ; ("loc_end", attr_payload)
          ; ("loc_ghost", attr_loc)
          ]

    method extension : extension -> extension -> json =
      fun (a, b) (a', b') ->
        let a = self#loc self#string a a' in
        let b = self#payload b b' in
        self#tuple [ a; b ]

    method attributes : attributes -> attributes -> json =
      self#list self#attribute

    method payload : payload -> payload -> json =
      fun x x' ->
        match (x, x') with
        | PStr a, PStr a' ->
          let a = self#structure a a' in
          self#constr "PStr" [ a ]
        | PSig a, PSig a' ->
          let a = self#signature a a' in
          self#constr "PSig" [ a ]
        | PTyp a, PTyp a' ->
          let a = self#core_type a a' in
          self#constr "PTyp" [ a ]
        | PPat (a, b), PPat (a', b') ->
          let a = self#pattern a a' acc in
          let b = self#option self#expression b b' acc in
          self#constr "PPat" [ a; b ]
        | _ ->
          diff_count <- diff_count + 1;
          let acc = add_diff "payload" (show_payload x) (show_payload x') acc in
          acc

    method core_type : core_type -> core_type -> json =
      fun { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes }
          { ptyp_desc = ptyp_desc'
          ; ptyp_loc = ptyp_loc'
          ; ptyp_loc_stack = ptyp_loc_stack'
          ; ptyp_attributes = ptyp_attributes'
          } ->
        let acc = self#core_type_desc ptyp_desc ptyp_desc' acc in
        let acc = self#location ptyp_loc ptyp_loc' acc in
        let acc = self#location_stack ptyp_loc_stack ptyp_loc_stack' acc in
        let acc = self#attributes ptyp_attributes ptyp_attributes' acc in
        acc

    method core_type_desc : core_type_desc -> core_type_desc -> json =
      fun x x' ->
        match (x, x') with
        | Ptyp_any, Ptyp_any -> acc
        | Ptyp_var a, Ptyp_var a' -> self#string a a' acc
        | Ptyp_arrow (a, b, c), Ptyp_arrow (a', b', c') ->
          let a = self#arg_label a a' acc in
          let b = self#core_type b b' acc in
          let c = self#core_type c c' acc in
          self#constr "Ptyp_arrow" [ a; b; c ]
        | Ptyp_tuple a, Ptyp_tuple a' -> self#list self#core_type a a' acc
        | Ptyp_constr (a, b), Ptyp_constr (a', b') ->
          let a = self#longident_loc a a' acc in
          let b = self#list self#core_type b b' acc in
          self#constr "Ptyp_constr" [ a; b ]
        | Ptyp_object (a, b), Ptyp_object (a', b') ->
          let a = self#list self#object_field a a' acc in
          let b = self#closed_flag b b' acc in
          self#constr "Ptyp_object" [ a; b ]
        | Ptyp_class (a, b), Ptyp_class (a', b') ->
          let a = self#longident_loc a a' acc in
          let b = self#list self#core_type b b' acc in
          self#constr "Ptyp_class" [ a; b ]
        | Ptyp_alias (a, b), Ptyp_alias (a', b') ->
          let a = self#core_type a a' acc in
          let b = self#string b b' acc in
          self#constr "Ptyp_alias" [ a; b ]
        | Ptyp_variant (a, b, c), Ptyp_variant (a', b', c') ->
          let a = self#list self#row_field a a' acc in
          let b = self#closed_flag b b' acc in
          let c = self#option (self#list self#label) c c' acc in
          self#constr "Ptyp_variant" [ a; b; c ]
        | Ptyp_poly (a, b), Ptyp_poly (a', b') ->
          let a = self#list (self#loc self#string) a a' acc in
          let b = self#core_type b b' acc in
          self#constr "Ptyp_poly" [ a; b ]
        | Ptyp_package a, Ptyp_package a' -> self#package_type a a' acc
        | Ptyp_extension a, Ptyp_extension a' -> self#extension a a' acc
        | _ -> raise Reparsing_error

    method package_type : package_type -> package_type -> json =
      fun (a, b) (a', b') ->
        let acc = self#longident_loc a a' acc in
        let acc =
          self#list
            (fun (a, b) (a', b') acc ->
              let acc = self#longident_loc a a' acc in
              let acc = self#core_type b b' acc in
              acc)
            b b' acc
        in
        acc

    method row_field : row_field -> row_field -> json =
      fun { prf_desc; prf_loc; prf_attributes }
          { prf_desc = prf_desc'
          ; prf_loc = prf_loc'
          ; prf_attributes = prf_attributes'
          } ->
        let acc = self#row_field_desc prf_desc prf_desc' acc in
        let acc = self#location prf_loc prf_loc' acc in
        let acc = self#attributes prf_attributes prf_attributes' acc in
        acc

    method row_field_desc : row_field_desc -> row_field_desc -> json =
      fun x x' ->
        match (x, x') with
        | Rtag (a, b, c), Rtag (a', b', c') ->
          let a = self#loc self#label a a' acc in
          let b = self#bool b b' acc in
          let c = self#list self#core_type c c' acc in
          self#constr "Rtag" [ a; b; c ]
        | Rinherit a, Rinherit a' ->
          let a = self#core_type a a' in
          self#constr "Rinherit" [ a ]
        | _ -> raise Reparsing_error

    method object_field : object_field -> object_field -> json =
      fun { pof_desc; pof_loc; pof_attributes }
          { pof_desc = pof_desc'
          ; pof_loc = pof_loc'
          ; pof_attributes = pof_attributes'
          } ->
        let acc = self#object_field_desc pof_desc pof_desc' acc in
        let acc = self#location pof_loc pof_loc' acc in
        let acc = self#attributes pof_attributes pof_attributes' acc in
        acc

    method object_field_desc : object_field_desc -> object_field_desc -> json =
      fun x x' ->
        match (x, x') with
        | Otag (a, b), Otag (a', b') ->
          let a = self#loc self#label a a' acc in
          let b = self#core_type b b' acc in
          self#constr "Otag" [ a; b ]
        | Oinherit a, Oinherit a' -> self#core_type a a' acc
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "object_field_desc" (show_object_field_desc x)
              (show_object_field_desc x')
              acc
          in
          acc

    method pattern : pattern -> pattern -> json =
      fun { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes }
          { ppat_desc = ppat_desc'
          ; ppat_loc = ppat_loc'
          ; ppat_loc_stack = ppat_loc_stack'
          ; ppat_attributes = ppat_attributes'
          } ->
        let acc = self#pattern_desc ppat_desc ppat_desc' acc in
        let acc = self#location ppat_loc ppat_loc' acc in
        let acc = self#location_stack ppat_loc_stack ppat_loc_stack' acc in
        let acc = self#attributes ppat_attributes ppat_attributes' acc in
        acc

    method pattern_desc : pattern_desc -> pattern_desc -> json =
      fun x x' ->
        match (x, x') with
        | Ppat_any, Ppat_any -> acc
        | Ppat_var a, Ppat_var a' -> self#loc self#string a a' acc
        | Ppat_alias (a, b), Ppat_alias (a', b') ->
          let a = self#pattern a a' acc in
          let b = self#loc self#string b b' acc in
          self#constr "Ppat_alias" [ a; b ]
        | Ppat_constant a, Ppat_constant a' -> self#constant a a' acc
        | Ppat_interval (a, b), Ppat_interval (a', b') ->
          let a = self#constant a a' acc in
          let b = self#constant b b' acc in
          self#constr "Ppat_interval" [ a; b ]
        | Ppat_tuple a, Ppat_tuple a' -> self#list self#pattern a a' acc
        | Ppat_construct (a, b), Ppat_construct (a', b') ->
          let a = self#longident_loc a a' acc in
          let b = self#option self#pattern b b' acc in
          self#constr "Ppat_construct" [ a; b ]
        | Ppat_variant (a, b), Ppat_variant (a', b') ->
          let a = self#label a a' acc in
          let b = self#option self#pattern b b' acc in
          self#constr "Ppat_variant" [ a; b ]
        | Ppat_record (a, b), Ppat_record (a', b') ->
          let acc =
            self#list
              (fun (a, b) (a', b') ->
                let acc = self#longident_loc a a' acc in
                let acc = self#pattern b b' acc in
                acc)
              a a' acc
          in
          let acc = self#closed_flag b b' acc in
          acc
        | Ppat_array a, Ppat_array a' -> self#list self#pattern a a' acc
        | Ppat_or (a, b), Ppat_or (a', b') ->
          let a = self#pattern a a' acc in
          let b = self#pattern b b' acc in
          self#constr "Ppat_or" [ a; b ]
        | Ppat_constraint (a, b), Ppat_constraint (a', b') ->
          let a = self#pattern a a' acc in
          let b = self#core_type b b' acc in
          self#constr "Ppat_constraint" [ a; b ]
        | Ppat_type a, Ppat_type a' -> self#longident_loc a a' acc
        | Ppat_lazy a, Ppat_lazy a' -> self#pattern a a' acc
        | Ppat_unpack a, Ppat_unpack a' ->
          self#loc (self#option self#string) a a' acc
        | Ppat_exception a, Ppat_exception a' -> self#pattern a a' acc
        | Ppat_extension a, Ppat_extension a' -> self#extension a a' acc
        | Ppat_open (a, b), Ppat_open (a', b') ->
          let a = self#longident_loc a a' acc in
          let b = self#pattern b b' acc in
          self#constr "Ppat_open" [ a; b ]
        (*BEGIN SPECIAL PATTERN CASES*)
        | Ppat_var _, Ppat_any ->
          let acc = self#pattern_desc x x acc in
          acc
        | Ppat_tuple [ exp1 ], ppat_desc' ->
          self#pattern_desc x
            (Ppat_tuple [ { exp1 with ppat_desc = ppat_desc' } ])
            acc
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "pattern_desc" (show_pattern_desc x) (show_pattern_desc x')
              acc
          in
          acc

    method expression : expression -> expression -> json =
      fun { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes }
          { pexp_desc = pexp_desc'
          ; pexp_loc = pexp_loc'
          ; pexp_loc_stack = pexp_loc_stack'
          ; pexp_attributes = pexp_attributes'
          } ->
        let acc = self#expression_desc pexp_desc pexp_desc' acc in
        let acc = self#location pexp_loc pexp_loc' acc in
        let acc = self#location_stack pexp_loc_stack pexp_loc_stack' acc in
        let acc = self#attributes pexp_attributes pexp_attributes' acc in
        acc

    method expression_desc : expression_desc -> expression_desc -> json =
      fun x x' ->
        match (x, x') with
        | Pexp_ident a, Pexp_ident a' -> self#longident_loc a a' acc
        | Pexp_constant a, Pexp_constant a' -> self#constant a a' acc
        | Pexp_let (a, b, c), Pexp_let (a', b', c') ->
          let a = self#rec_flag a a' acc in
          let b = self#list self#value_binding b b' acc in
          let c = self#expression c c' acc in
          self#constr "Pexp_let" [ a; b; c ]
        | Pexp_function a, Pexp_function a' -> self#cases a a' acc
        | Pexp_fun (a, b, c, d), Pexp_fun (a', b', c', d') ->
          let acc = self#arg_label a a' acc in
          let acc = self#option self#expression b b' acc in
          let acc = self#pattern c c' acc in
          let acc = self#expression d d' acc in
          acc
        | Pexp_apply (a, b), Pexp_apply (a', b') ->
          let acc = self#expression a a' acc in
          let acc =
            self#list
              (fun (a, b) (a', b') ->
                let acc = self#arg_label a a' acc in
                let acc = self#expression b b' acc in
                acc)
              b b' acc
          in
          acc
        | Pexp_match (a, b), Pexp_match (a', b') ->
          let a = self#expression a a' acc in
          let b = self#cases b b' acc in
          self#constr "Pexp_match" [ a; b ]
        | Pexp_try (a, b), Pexp_try (a', b') ->
          let a = self#expression a a' acc in
          let b = self#cases b b' acc in
          self#constr "Pexp_try" [ a; b ]
        | Pexp_tuple a, Pexp_tuple a' -> self#list self#expression a a' acc
        | Pexp_construct (a, b), Pexp_construct (a', b') ->
          let a = self#longident_loc a a' acc in
          let b = self#option self#expression b b' acc in
          self#constr "Pexp_construct" [ a; b ]
        | Pexp_variant (a, b), Pexp_variant (a', b') ->
          let a = self#label a a' acc in
          let b = self#option self#expression b b' acc in
          self#constr "Pexp_variant" [ a; b ]
        | Pexp_record (a, b), Pexp_record (a', b') ->
          let acc =
            self#list
              (fun (a, b) (a', b') ->
                let acc = self#longident_loc a a' acc in
                let acc = self#expression b b' acc in
                acc)
              a a' acc
          in
          let acc = self#option self#expression b b' acc in
          acc
        | Pexp_field (a, b), Pexp_field (a', b') ->
          let a = self#expression a a' acc in
          let b = self#longident_loc b b' acc in
          self#constr "Pexp_field" [ a; b ]
        | Pexp_setfield (a, b, c), Pexp_setfield (a', b', c') ->
          let a = self#expression a a' acc in
          let b = self#longident_loc b b' acc in
          let c = self#expression c c' acc in
          self#constr "Pexp_setfield" [ a; b; c ]
        | Pexp_array a, Pexp_array a' -> self#list self#expression a a' acc
        | Pexp_ifthenelse (a, b, c), Pexp_ifthenelse (a', b', c') ->
          let a = self#expression a a' acc in
          let b = self#expression b b' acc in
          let c = self#option self#expression c c' acc in
          self#constr "Pexp_ifthenelse" [ a; b; c ]
        | Pexp_sequence (a, b), Pexp_sequence (a', b') ->
          let a = self#expression a a' acc in
          let b = self#expression b b' acc in
          self#constr "Pexp_sequence" [ a; b ]
        | Pexp_while (a, b), Pexp_while (a', b') ->
          let a = self#expression a a' acc in
          let b = self#expression b b' acc in
          self#constr "Pexp_while" [ a; b ]
        | Pexp_for (a, b, c, d, e), Pexp_for (a', b', c', d', e') ->
          let acc = self#pattern a a' acc in
          let acc = self#expression b b' acc in
          let acc = self#expression c c' acc in
          let acc = self#direction_flag d d' acc in
          let acc = self#expression e e' acc in
          acc
        | Pexp_constraint (a, b), Pexp_constraint (a', b') ->
          let a = self#expression a a' acc in
          let b = self#core_type b b' acc in
          self#constr "Pexp_constraint" [ a; b ]
        | Pexp_coerce (a, b, c), Pexp_coerce (a', b', c') ->
          let a = self#expression a a' acc in
          let b = self#option self#core_type b b' acc in
          let c = self#core_type c c' acc in
          self#constr "Pexp_coerce" [ a; b; c ]
        | Pexp_send (a, b), Pexp_send (a', b') ->
          let a = self#expression a a' acc in
          let b = self#loc self#label b b' acc in
          self#constr "Pexp_send" [ a; b ]
        | Pexp_new a, Pexp_new a' -> self#longident_loc a a' acc
        | Pexp_setinstvar (a, b), Pexp_setinstvar (a', b') ->
          let a = self#loc self#label a a' acc in
          let b = self#expression b b' acc in
          self#constr "Pexp_setinstvar" [ a; b ]
        | Pexp_override a, Pexp_override a' ->
          self#list
            (fun (a, b) (a', b') ->
              let acc = self#loc self#label a a' acc in
              let acc = self#expression b b' acc in
              acc)
            a a' acc
        | Pexp_letmodule (a, b, c), Pexp_letmodule (a', b', c') ->
          let a = self#loc (self#option self#string) a a' acc in
          let b = self#module_expr b b' acc in
          let c = self#expression c c' acc in
          self#constr "Pexp_letmodule" [ a; b; c ]
        | Pexp_letexception (a, b), Pexp_letexception (a', b') ->
          let a = self#extension_constructor a a' acc in
          let b = self#expression b b' acc in
          self#constr "Pexp_letexception" [ a; b ]
        | Pexp_assert a, Pexp_assert a' -> self#expression a a' acc
        | Pexp_lazy a, Pexp_lazy a' -> self#expression a a' acc
        | Pexp_poly (a, b), Pexp_poly (a', b') ->
          let a = self#expression a a' acc in
          let b = self#option self#core_type b b' acc in
          self#constr "Pexp_poly" [ a; b ]
        | Pexp_object a, Pexp_object a' -> self#class_structure a a' acc
        | Pexp_newtype (a, b), Pexp_newtype (a', b') ->
          let a = self#loc self#string a a' acc in
          let b = self#expression b b' acc in
          self#constr "Pexp_newtype" [ a; b ]
        | Pexp_pack a, Pexp_pack a' -> self#module_expr a a' acc
        | Pexp_open (a, b), Pexp_open (a', b') ->
          let a = self#open_declaration a a' acc in
          let b = self#expression b b' acc in
          self#constr "Pexp_open" [ a; b ]
        | Pexp_letop a, Pexp_letop a' -> self#letop a a' acc
        | Pexp_extension a, Pexp_extension a' -> self#extension a a' acc
        | Pexp_unreachable, Pexp_unreachable -> acc
        (* BEGIN SPECIAL CASES*)
        (*Note: in the case where compiler provides less information (e.g.
          unwrapping Pexp_apply, Pexp_tuple [x] ) we keep the original (ppx)
          locations. However, it might be interesing to upstream the locations
          from the reparsed node. *)
        | ( Pexp_apply (({ pexp_desc = Pexp_newtype _; _ } as exp1), al_exp_list)
          , Pexp_newtype _ ) ->
          let acc =
            self#expression_desc x
              (Pexp_apply ({ exp1 with pexp_desc = x' }, al_exp_list))
              acc
          in
          acc
          (*around 300*)
        | ( Pexp_newtype (a, b)
          , Pexp_apply ({ pexp_desc = Pexp_newtype (a', b'); _ }, _) ) ->
          let acc = self#loc self#string a a' acc in
          let acc = self#expression b b' acc in
          acc
          (* 7881 to 7833 (48)*)
        | ( Pexp_constraint (a, b)
          , Pexp_apply ({ pexp_desc = Pexp_constraint (a', b'); _ }, _) ) ->
          let acc = self#expression a a' acc in
          let acc = self#core_type b b' acc in
          acc
          (* 7833 to 7672 (161)*)
        | ( Pexp_open (a, b)
          , Pexp_constraint ({ pexp_desc = Pexp_open (a', b'); _ }, _) ) ->
          let acc = self#open_declaration a a' acc in
          let acc = self#expression b b' acc in
          acc
        (* 7672 to 6612 (1060)*)
        | ( Pexp_fun (a, b, c, d)
          , Pexp_apply ({ pexp_desc = Pexp_fun (a', b', c', d'); _ }, _) ) ->
          let acc = self#arg_label a a' acc in
          let acc = self#option self#expression b b' acc in
          let acc = self#pattern c c' acc in
          let acc = self#expression d d' acc in
          acc
        (* 6612 to 6335 (277)*)
        | ( Pexp_fun (a, b, c, d)
          , Pexp_constraint ({ pexp_desc = Pexp_fun (a', b', c', d'); _ }, _) )
          ->
          let acc = self#arg_label a a' acc in
          let acc = self#option self#expression b b' acc in
          let acc = self#pattern c c' acc in
          let acc = self#expression d d' acc in
          acc
        (* 6335 to 5734 (601)*)
        | pexp_desc, Pexp_constraint ({ pexp_desc = pexp_desc'; _ }, _) ->
          let acc = self#expression_desc pexp_desc pexp_desc' acc in
          acc
        (* 5734 to 5119 (615)*)
        | pexp_desc, Pexp_poly ({ pexp_desc = pexp_desc'; _ }, _) ->
          let acc = self#expression_desc pexp_desc pexp_desc' acc in
          acc
        (*5119 to 80 (5039)*)
        | ( oexpr_desc
          , Pexp_newtype
              ( lloc1
              , ({ pexp_desc =
                     Pexp_newtype
                       ( lloc2
                       , ({ pexp_desc =
                              Pexp_newtype
                                ( lloc3
                                , { pexp_desc =
                                      Pexp_sequence
                                        ( ({ pexp_desc =
                                               Pexp_apply (e1, arg_lablel_e_list)
                                           ; _
                                           } as seqexp1)
                                        , e2 )
                                  ; _
                                  } )
                          ; _
                          } as ntrec2) )
                 ; _
                 } as ntrec1) ) ) ->
          let normalized_newtypes =
            Pexp_newtype
              ( lloc1
              , { ntrec1 with
                  pexp_desc =
                    Pexp_newtype
                      ( lloc2
                      , { ntrec2 with pexp_desc = Pexp_newtype (lloc3, e1) } )
                } )
          in
          let normalized_apply =
            Pexp_apply
              ({ e1 with pexp_desc = normalized_newtypes }, arg_lablel_e_list)
          in
          let normalized_pexp_desc2 =
            Pexp_sequence ({ seqexp1 with pexp_desc = normalized_apply }, e2)
          in
          let acc = self#expression_desc oexpr_desc normalized_pexp_desc2 acc in
          acc
        (* 78 to 18 (50)*)
        | Pexp_tuple [ exp1 ], pexp_desc' ->
          self#expression_desc x
            (Pexp_tuple [ { exp1 with pexp_desc = pexp_desc' } ])
            acc
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "expression_desc" (show_expression_desc x)
              (show_expression_desc x') acc
          in
          acc

    method case : case -> case -> json =
      fun { pc_lhs; pc_guard; pc_rhs }
          { pc_lhs = pc_lhs'; pc_guard = pc_guard'; pc_rhs = pc_rhs' } ->
        let acc = self#pattern pc_lhs pc_lhs' acc in
        let acc = self#option self#expression pc_guard pc_guard' acc in
        let acc = self#expression pc_rhs pc_rhs' acc in
        acc

    method letop : letop -> letop -> json =
      fun { let_; ands; body } { let_ = let_'; ands = ands'; body = body' } ->
        let acc = self#binding_op let_ let_' acc in
        let acc = self#list self#binding_op ands ands' acc in
        let acc = self#expression body body' acc in
        acc

    method binding_op : binding_op -> binding_op -> json =
      fun { pbop_op; pbop_pat; pbop_exp; pbop_loc }
          { pbop_op = pbop_op'
          ; pbop_pat = pbop_pat'
          ; pbop_exp = pbop_exp'
          ; pbop_loc = pbop_loc'
          } ->
        let acc = self#loc self#string pbop_op pbop_op' acc in
        let acc = self#pattern pbop_pat pbop_pat' acc in
        let acc = self#expression pbop_exp pbop_exp' acc in
        let acc = self#location pbop_loc pbop_loc' acc in
        acc

    method value_description : value_description -> value_description -> json =
      fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }
          { pval_name = pval_name'
          ; pval_type = pval_type'
          ; pval_prim = pval_prim'
          ; pval_attributes = pval_attributes'
          ; pval_loc = pval_loc'
          } ->
        let acc = self#loc self#string pval_name pval_name' acc in
        let acc = self#core_type pval_type pval_type' acc in
        let acc = self#list self#string pval_prim pval_prim' acc in
        let acc = self#attributes pval_attributes pval_attributes' acc in
        let acc = self#location pval_loc pval_loc' acc in
        acc

    method type_declaration : type_declaration -> type_declaration -> json =
      fun { ptype_name
          ; ptype_params
          ; ptype_cstrs
          ; ptype_kind
          ; ptype_private
          ; ptype_manifest
          ; ptype_attributes
          ; ptype_loc
          }
          { ptype_name = ptype_name'
          ; ptype_params = ptype_params'
          ; ptype_cstrs = ptype_cstrs'
          ; ptype_kind = ptype_kind'
          ; ptype_private = ptype_private'
          ; ptype_manifest = ptype_manifest'
          ; ptype_attributes = ptype_attributes'
          ; ptype_loc = ptype_loc'
          } ->
        let acc = self#loc self#string ptype_name ptype_name' acc in
        let acc =
          self#list
            (fun (a, b) (a', b') ->
              let acc = self#core_type a a' acc in
              let acc =
                (fun (a, b) (a', b') ->
                  let acc = self#variance a a' acc in
                  let acc = self#injectivity b b' acc in
                  acc)
                  b b' acc
              in
              acc)
            ptype_params ptype_params' acc
        in
        let acc =
          self#list
            (fun (a, b, c) (a', b', c') ->
              let acc = self#core_type a a' acc in
              let acc = self#core_type b b' acc in
              let acc = self#location c c' acc in
              acc)
            ptype_cstrs ptype_cstrs' acc
        in
        let acc = self#type_kind ptype_kind ptype_kind' acc in
        let acc = self#private_flag ptype_private ptype_private' acc in
        let acc =
          self#option self#core_type ptype_manifest ptype_manifest' acc
        in
        let acc = self#attributes ptype_attributes ptype_attributes' acc in
        let acc = self#location ptype_loc ptype_loc' acc in
        acc

    method type_kind : type_kind -> type_kind -> json =
      fun x x' ->
        match (x, x') with
        | Ptype_abstract, Ptype_abstract -> acc
        | Ptype_variant a, Ptype_variant a' ->
          self#list self#constructor_declaration a a' acc
        | Ptype_record a, Ptype_record a' ->
          self#list self#label_declaration a a' acc
        | Ptype_open, Ptype_open -> acc
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "type_kind" (show_type_kind x) (show_type_kind x') acc
          in
          acc

    method label_declaration : label_declaration -> label_declaration -> json =
      fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }
          { pld_name = pld_name'
          ; pld_mutable = pld_mutable'
          ; pld_type = pld_type'
          ; pld_loc = pld_loc'
          ; pld_attributes = pld_attributes'
          } ->
        let acc = self#loc self#string pld_name pld_name' acc in
        let acc = self#mutable_flag pld_mutable pld_mutable' acc in
        let acc = self#core_type pld_type pld_type' acc in
        let acc = self#location pld_loc pld_loc' acc in
        let acc = self#attributes pld_attributes pld_attributes' acc in
        acc

    method constructor_declaration
        : constructor_declaration -> constructor_declaration -> json =
      fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }
          { pcd_name = pcd_name'
          ; pcd_args = pcd_args'
          ; pcd_res = pcd_res'
          ; pcd_loc = pcd_loc'
          ; pcd_attributes = pcd_attributes'
          } ->
        let acc = self#loc self#string pcd_name pcd_name' acc in
        let acc = self#constructor_arguments pcd_args pcd_args' acc in
        let acc = self#option self#core_type pcd_res pcd_res' acc in
        let acc = self#location pcd_loc pcd_loc' acc in
        let acc = self#attributes pcd_attributes pcd_attributes' acc in
        acc

    method constructor_arguments
        : constructor_arguments -> constructor_arguments -> json =
      fun x x' ->
        match (x, x') with
        | Pcstr_tuple a, Pcstr_tuple a' -> self#list self#core_type a a' acc
        | Pcstr_record a, Pcstr_record a' ->
          self#list self#label_declaration a a' acc
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "constructor_arguments"
              (show_constructor_arguments x)
              (show_constructor_arguments x')
              acc
          in
          acc

    method type_extension : type_extension -> type_extension -> json =
      fun { ptyext_path
          ; ptyext_params
          ; ptyext_constructors
          ; ptyext_private
          ; ptyext_loc
          ; ptyext_attributes
          }
          { ptyext_path = ptyext_path'
          ; ptyext_params = ptyext_params'
          ; ptyext_constructors = ptyext_constructors'
          ; ptyext_private = ptyext_private'
          ; ptyext_loc = ptyext_loc'
          ; ptyext_attributes = ptyext_attributes'
          } ->
        let acc = self#longident_loc ptyext_path ptyext_path' acc in
        let acc =
          self#list
            (fun (a, b) (a', b') ->
              let acc = self#core_type a a' acc in
              let acc =
                (fun (a, b) (a', b') ->
                  let acc = self#variance a a' acc in
                  let acc = self#injectivity b b' acc in
                  acc)
                  b b' acc
              in
              acc)
            ptyext_params ptyext_params' acc
        in
        let acc =
          self#list self#extension_constructor ptyext_constructors
            ptyext_constructors' acc
        in
        let acc = self#private_flag ptyext_private ptyext_private' acc in
        let acc = self#location ptyext_loc ptyext_loc' acc in
        let acc = self#attributes ptyext_attributes ptyext_attributes' acc in
        acc

    method extension_constructor
        : extension_constructor -> extension_constructor -> json =
      fun { pext_name; pext_kind; pext_loc; pext_attributes }
          { pext_name = pext_name'
          ; pext_kind = pext_kind'
          ; pext_loc = pext_loc'
          ; pext_attributes = pext_attributes'
          } ->
        let acc = self#loc self#string pext_name pext_name' acc in
        let acc = self#extension_constructor_kind pext_kind pext_kind' acc in
        let acc = self#location pext_loc pext_loc' acc in
        let acc = self#attributes pext_attributes pext_attributes' acc in
        acc

    method type_exception : type_exception -> type_exception -> json =
      fun { ptyexn_constructor; ptyexn_loc; ptyexn_attributes }
          { ptyexn_constructor = ptyexn_constructor'
          ; ptyexn_loc = ptyexn_loc'
          ; ptyexn_attributes = ptyexn_attributes'
          } ->
        let acc =
          self#extension_constructor ptyexn_constructor ptyexn_constructor' acc
        in
        let acc = self#location ptyexn_loc ptyexn_loc' acc in
        let acc = self#attributes ptyexn_attributes ptyexn_attributes' acc in
        acc

    method extension_constructor_kind
        : extension_constructor_kind -> extension_constructor_kind -> json =
      fun x x' ->
        match (x, x') with
        | Pext_decl (a, b), Pext_decl (a', b') ->
          let a = self#constructor_arguments a a' acc in
          let b = self#option self#core_type b b' acc in
          self#constr "Pext_decl" [ a; b ]
        | Pext_rebind a, Pext_rebind a' -> self#longident_loc a a' acc
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "extension_constructor_kind"
              (show_extension_constructor_kind x)
              (show_extension_constructor_kind x')
              acc
          in
          acc

    method class_type : class_type -> class_type -> json =
      fun { pcty_desc; pcty_loc; pcty_attributes }
          { pcty_desc = pcty_desc'
          ; pcty_loc = pcty_loc'
          ; pcty_attributes = pcty_attributes'
          } ->
        let acc = self#class_type_desc pcty_desc pcty_desc' acc in
        let acc = self#location pcty_loc pcty_loc' acc in
        let acc = self#attributes pcty_attributes pcty_attributes' acc in
        acc

    method class_type_desc : class_type_desc -> class_type_desc -> json =
      fun x x' ->
        match (x, x') with
        | Pcty_constr (a, b), Pcty_constr (a', b') ->
          let a = self#longident_loc a a' acc in
          let b = self#list self#core_type b b' acc in
          self#constr "Pcty_constr" [ a; b ]
        | Pcty_signature a, Pcty_signature a' -> self#class_signature a a' acc
        | Pcty_arrow (a, b, c), Pcty_arrow (a', b', c') ->
          let a = self#arg_label a a' acc in
          let b = self#core_type b b' acc in
          let c = self#class_type c c' acc in
          self#constr "Pcty_arrow" [ a; b; c ]
        | Pcty_extension a, Pcty_extension a' -> self#extension a a' acc
        | Pcty_open (a, b), Pcty_open (a', b') ->
          let a = self#open_description a a' acc in
          let b = self#class_type b b' acc in
          self#constr "Pcty_open" [ a; b ]
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "class_type_desc" (show_class_type_desc x)
              (show_class_type_desc x') acc
          in
          acc

    method class_signature : class_signature -> class_signature -> json =
      fun { pcsig_self; pcsig_fields }
          { pcsig_self = pcsig_self'; pcsig_fields = pcsig_fields' } ->
        let acc = self#core_type pcsig_self pcsig_self' acc in
        let acc =
          self#list self#class_type_field pcsig_fields pcsig_fields' acc
        in
        acc

    method class_type_field : class_type_field -> class_type_field -> json =
      fun { pctf_desc; pctf_loc; pctf_attributes }
          { pctf_desc = pctf_desc'
          ; pctf_loc = pctf_loc'
          ; pctf_attributes = pctf_attributes'
          } ->
        let acc = self#class_type_field_desc pctf_desc pctf_desc' acc in
        let acc = self#location pctf_loc pctf_loc' acc in
        let acc = self#attributes pctf_attributes pctf_attributes' acc in
        acc

    method class_type_field_desc
        : class_type_field_desc -> class_type_field_desc -> json =
      fun x x' ->
        match (x, x') with
        | Pctf_inherit a, Pctf_inherit a' -> self#class_type a a' acc
        | Pctf_val a, Pctf_val a' ->
          (fun (a, b, c, d) (a', b', c', d') ->
            let acc = self#loc self#label a a' acc in
            let acc = self#mutable_flag b b' acc in
            let acc = self#virtual_flag c c' acc in
            let acc = self#core_type d d' acc in
            acc)
            a a' acc
        | Pctf_method a, Pctf_method a' ->
          (fun (a, b, c, d) (a', b', c', d') ->
            let acc = self#loc self#label a a' acc in
            let acc = self#private_flag b b' acc in
            let acc = self#virtual_flag c c' acc in
            let acc = self#core_type d d' acc in
            acc)
            a a' acc
        | Pctf_constraint a, Pctf_constraint a' ->
          (fun (a, b) (a', b') ->
            let acc = self#core_type a a' acc in
            let acc = self#core_type b b' acc in
            acc)
            a a' acc
        | Pctf_attribute a, Pctf_attribute a' -> self#attribute a a' acc
        | Pctf_extension a, Pctf_extension a' -> self#extension a a' acc
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "class_type_field_desc"
              (show_class_type_field_desc x)
              (show_class_type_field_desc x')
              acc
          in
          acc

    method class_infos
        : 'a. ('a -> 'a -> json) -> 'a class_infos -> 'a class_infos -> json =
      fun _a
          { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
          { pci_virt = pci_virt'
          ; pci_params = pci_params'
          ; pci_name = pci_name'
          ; pci_expr = pci_expr'
          ; pci_loc = pci_loc'
          ; pci_attributes = pci_attributes'
          } ->
        let acc = self#virtual_flag pci_virt pci_virt' acc in
        let acc =
          self#list
            (fun (a, b) (a', b') ->
              let acc = self#core_type a a' acc in
              let acc =
                (fun (a, b) (a', b') ->
                  let acc = self#variance a a' acc in
                  let acc = self#injectivity b b' acc in
                  acc)
                  b b' acc
              in
              acc)
            pci_params pci_params' acc
        in
        let acc = self#loc self#string pci_name pci_name' acc in
        let acc = _a pci_expr pci_expr' acc in
        let acc = self#location pci_loc pci_loc' acc in
        let acc = self#attributes pci_attributes pci_attributes' acc in
        acc

    method class_description : class_description -> class_description -> json =
      self#class_infos self#class_type

    method class_type_declaration
        : class_type_declaration -> class_type_declaration -> json =
      self#class_infos self#class_type

    method class_expr : class_expr -> class_expr -> json =
      fun { pcl_desc; pcl_loc; pcl_attributes }
          { pcl_desc = pcl_desc'
          ; pcl_loc = pcl_loc'
          ; pcl_attributes = pcl_attributes'
          } ->
        let acc = self#class_expr_desc pcl_desc pcl_desc' acc in
        let acc = self#location pcl_loc pcl_loc' acc in
        let acc = self#attributes pcl_attributes pcl_attributes' acc in
        acc

    method class_expr_desc : class_expr_desc -> class_expr_desc -> json =
      fun x x' ->
        match (x, x') with
        | Pcl_constr (a, b), Pcl_constr (a', b') ->
          let a = self#longident_loc a a' acc in
          let b = self#list self#core_type b b' acc in
          self#constr "Pcl_constr" [ a; b ]
        | Pcl_structure a, Pcl_structure a' -> self#class_structure a a' acc
        | Pcl_fun (a, b, c, d), Pcl_fun (a', b', c', d') ->
          let acc = self#arg_label a a' acc in
          let acc = self#option self#expression b b' acc in
          let acc = self#pattern c c' acc in
          let acc = self#class_expr d d' acc in
          acc
        | Pcl_apply (a, b), Pcl_apply (a', b') ->
          let acc = self#class_expr a a' acc in
          let acc =
            self#list
              (fun (a, b) (a', b') ->
                let acc = self#arg_label a a' acc in
                let acc = self#expression b b' acc in
                acc)
              b b' acc
          in
          acc
        | Pcl_let (a, b, c), Pcl_let (a', b', c') ->
          let a = self#rec_flag a a' acc in
          let b = self#list self#value_binding b b' acc in
          let c = self#class_expr c c' acc in
          self#constr "Pcl_let" [ a; b; c ]
        | Pcl_constraint (a, b), Pcl_constraint (a', b') ->
          let a = self#class_expr a a' acc in
          let b = self#class_type b b' acc in
          self#constr "Pcl_constraint" [ a; b ]
        | Pcl_extension a, Pcl_extension a' -> self#extension a a' acc
        | Pcl_open (a, b), Pcl_open (a', b') ->
          let a = self#open_description a a' acc in
          let b = self#class_expr b b' acc in
          self#constr "Pcl_open" [ a; b ]
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "class_expr_desc" (show_class_expr_desc x)
              (show_class_expr_desc x') acc
          in
          acc

    method class_structure : class_structure -> class_structure -> json =
      fun { pcstr_self; pcstr_fields }
          { pcstr_self = pcstr_self'; pcstr_fields = pcstr_fields' } ->
        let acc = self#pattern pcstr_self pcstr_self' acc in
        let acc = self#list self#class_field pcstr_fields pcstr_fields' acc in
        acc

    method class_field : class_field -> class_field -> json =
      fun { pcf_desc; pcf_loc; pcf_attributes }
          { pcf_desc = pcf_desc'
          ; pcf_loc = pcf_loc'
          ; pcf_attributes = pcf_attributes'
          } ->
        let acc = self#class_field_desc pcf_desc pcf_desc' acc in
        let acc = self#location pcf_loc pcf_loc' acc in
        let acc = self#attributes pcf_attributes pcf_attributes' acc in
        acc

    method class_field_desc : class_field_desc -> class_field_desc -> json =
      fun x x' ->
        match (x, x') with
        | Pcf_inherit (a, b, c), Pcf_inherit (a', b', c') ->
          let a = self#override_flag a a' acc in
          let b = self#class_expr b b' acc in
          let c = self#option (self#loc self#string) c c' acc in
          self#constr "Pcf_inherit" [ a; b; c ]
        | Pcf_val a, Pcf_val a' ->
          (fun (a, b, c) (a', b', c') ->
            let acc = self#loc self#label a a' acc in
            let acc = self#mutable_flag b b' acc in
            let acc = self#class_field_kind c c' acc in
            acc)
            a a' acc
        | Pcf_method a, Pcf_method a' ->
          (fun (a, b, c) (a', b', c') ->
            let acc = self#loc self#label a a' acc in
            let acc = self#private_flag b b' acc in
            let acc = self#class_field_kind c c' acc in
            acc)
            a a' acc
        | Pcf_constraint a, Pcf_constraint a' ->
          (fun (a, b) (a', b') ->
            let acc = self#core_type a a' acc in
            let acc = self#core_type b b' acc in
            acc)
            a a' acc
        | Pcf_initializer a, Pcf_initializer a' -> self#expression a a' acc
        | Pcf_attribute a, Pcf_attribute a' -> self#attribute a a' acc
        | Pcf_extension a, Pcf_extension a' -> self#extension a a' acc
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "class_field_desc" (show_class_field_desc x)
              (show_class_field_desc x') acc
          in
          acc

    method class_field_kind : class_field_kind -> class_field_kind -> json =
      fun x x' ->
        match (x, x') with
        | Cfk_virtual a, Cfk_virtual a' -> self#core_type a a' acc
        | Cfk_concrete (a, b), Cfk_concrete (a', b') ->
          let a = self#override_flag a a' acc in
          let b = self#expression b b' acc in
          self#constr "Cfk_concrete" [ a; b ]
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "class_field_kind" (show_class_field_kind x)
              (show_class_field_kind x') acc
          in
          acc

    method class_declaration : class_declaration -> class_declaration -> json =
      self#class_infos self#class_expr

    method module_type : module_type -> module_type -> json =
      fun { pmty_desc; pmty_loc; pmty_attributes }
          { pmty_desc = pmty_desc'
          ; pmty_loc = pmty_loc'
          ; pmty_attributes = pmty_attributes'
          } ->
        let acc = self#module_type_desc pmty_desc pmty_desc' acc in
        let acc = self#location pmty_loc pmty_loc' acc in
        let acc = self#attributes pmty_attributes pmty_attributes' acc in
        acc

    method module_type_desc : module_type_desc -> module_type_desc -> json =
      fun x x' ->
        match (x, x') with
        | Pmty_ident a, Pmty_ident a' -> self#longident_loc a a' acc
        | Pmty_signature a, Pmty_signature a' -> self#signature a a' acc
        | Pmty_functor (a, b), Pmty_functor (a', b') ->
          let a = self#functor_parameter a a' acc in
          let b = self#module_type b b' acc in
          self#constr "Pmty_functor" [ a; b ]
        | Pmty_with (a, b), Pmty_with (a', b') ->
          let a = self#module_type a a' acc in
          let b = self#list self#with_constraint b b' acc in
          self#constr "Pmty_with" [ a; b ]
        | Pmty_typeof a, Pmty_typeof a' -> self#module_expr a a' acc
        | Pmty_extension a, Pmty_extension a' -> self#extension a a' acc
        | Pmty_alias a, Pmty_alias a' -> self#longident_loc a a' acc
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "module_type_desc" (show_module_type_desc x)
              (show_module_type_desc x') acc
          in
          acc

    method functor_parameter : functor_parameter -> functor_parameter -> json =
      fun x x' ->
        match (x, x') with
        | Unit, Unit -> acc
        | Named (a, b), Named (a', b') ->
          let a = self#loc (self#option self#string) a a' acc in
          let b = self#module_type b b' acc in
          self#constr "Named" [ a; b ]
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "functor_parameter" (show_functor_parameter x)
              (show_functor_parameter x')
              acc
          in
          acc

    method signature : signature -> signature -> json =
      self#list self#signature_item

    method signature_item : signature_item -> signature_item -> json =
      fun { psig_desc; psig_loc }
          { psig_desc = psig_desc'; psig_loc = psig_loc' } ->
        let acc = self#signature_item_desc psig_desc psig_desc' acc in
        let acc = self#location psig_loc psig_loc' acc in
        acc

    method signature_item_desc
        : signature_item_desc -> signature_item_desc -> json =
      fun x x' ->
        match (x, x') with
        | Psig_value a, Psig_value a' -> self#value_description a a' acc
        | Psig_type (a, b), Psig_type (a', b') ->
          let a = self#rec_flag a a' acc in
          let b = self#list self#type_declaration b b' acc in
          self#constr "Psig_type" [ a; b ]
        | Psig_typesubst a, Psig_typesubst a' ->
          self#list self#type_declaration a a' acc
        | Psig_typext a, Psig_typext a' -> self#type_extension a a' acc
        | Psig_exception a, Psig_exception a' -> self#type_exception a a' acc
        | Psig_module a, Psig_module a' -> self#module_declaration a a' acc
        | Psig_modsubst a, Psig_modsubst a' -> self#module_substitution a a' acc
        | Psig_recmodule a, Psig_recmodule a' ->
          self#list self#module_declaration a a' acc
        | Psig_modtype a, Psig_modtype a' ->
          self#module_type_declaration a a' acc
        | Psig_open a, Psig_open a' -> self#open_description a a' acc
        | Psig_include a, Psig_include a' -> self#include_description a a' acc
        | Psig_class a, Psig_class a' ->
          self#list self#class_description a a' acc
        | Psig_class_type a, Psig_class_type a' ->
          self#list self#class_type_declaration a a' acc
        | Psig_attribute a, Psig_attribute a' -> self#attribute a a' acc
        | Psig_extension (a, b), Psig_extension (a', b') ->
          let a = self#extension a a' acc in
          let b = self#attributes b b' acc in
          self#constr "Psig_extension" [ a; b ]
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "signature_item_desc"
              (show_signature_item_desc x)
              (show_signature_item_desc x')
              acc
          in
          acc

    method module_declaration : module_declaration -> module_declaration -> json
        =
      fun { pmd_name; pmd_type; pmd_attributes; pmd_loc }
          { pmd_name = pmd_name'
          ; pmd_type = pmd_type'
          ; pmd_attributes = pmd_attributes'
          ; pmd_loc = pmd_loc'
          } ->
        let acc = self#loc (self#option self#string) pmd_name pmd_name' acc in
        let acc = self#module_type pmd_type pmd_type' acc in
        let acc = self#attributes pmd_attributes pmd_attributes' acc in
        let acc = self#location pmd_loc pmd_loc' acc in
        acc

    method module_substitution
        : module_substitution -> module_substitution -> json =
      fun { pms_name; pms_manifest; pms_attributes; pms_loc }
          { pms_name = pms_name'
          ; pms_manifest = pms_manifest'
          ; pms_attributes = pms_attributes'
          ; pms_loc = pms_loc'
          } ->
        let acc = self#loc self#string pms_name pms_name' acc in
        let acc = self#longident_loc pms_manifest pms_manifest' acc in
        let acc = self#attributes pms_attributes pms_attributes' acc in
        let acc = self#location pms_loc pms_loc' acc in
        acc

    method module_type_declaration
        : module_type_declaration -> module_type_declaration -> json =
      fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }
          { pmtd_name = pmtd_name'
          ; pmtd_type = pmtd_type'
          ; pmtd_attributes = pmtd_attributes'
          ; pmtd_loc = pmtd_loc'
          } ->
        let acc = self#loc self#string pmtd_name pmtd_name' acc in
        let acc = self#option self#module_type pmtd_type pmtd_type' acc in
        let acc = self#attributes pmtd_attributes pmtd_attributes' acc in
        let acc = self#location pmtd_loc pmtd_loc' acc in
        acc

    method open_infos
        : 'a. ('a -> 'a -> json) -> 'a open_infos -> 'a open_infos -> json =
      fun _a { popen_expr; popen_override; popen_loc; popen_attributes }
          { popen_expr = popen_expr'
          ; popen_override = popen_override'
          ; popen_loc = popen_loc'
          ; popen_attributes = popen_attributes'
          } ->
        let acc = _a popen_expr popen_expr' acc in
        let acc = self#override_flag popen_override popen_override' acc in
        let acc = self#location popen_loc popen_loc' acc in
        let acc = self#attributes popen_attributes popen_attributes' acc in
        acc

    method open_description : open_description -> open_description -> json =
      self#open_infos self#longident_loc

    method open_declaration : open_declaration -> open_declaration -> json =
      self#open_infos self#module_expr

    method include_infos
        : 'a. ('a -> 'a -> json) -> 'a include_infos -> 'a include_infos -> json
        =
      fun _a { pincl_mod; pincl_loc; pincl_attributes }
          { pincl_mod = pincl_mod'
          ; pincl_loc = pincl_loc'
          ; pincl_attributes = pincl_attributes'
          } ->
        let acc = _a pincl_mod pincl_mod' acc in
        let acc = self#location pincl_loc pincl_loc' acc in
        let acc = self#attributes pincl_attributes pincl_attributes' acc in
        acc

    method include_description
        : include_description -> include_description -> json =
      self#include_infos self#module_type

    method include_declaration
        : include_declaration -> include_declaration -> json =
      self#include_infos self#module_expr

    method with_constraint : with_constraint -> with_constraint -> json =
      fun x x' ->
        match (x, x') with
        | Pwith_type (a, b), Pwith_type (a', b') ->
          let a = self#longident_loc a a' acc in
          let b = self#type_declaration b b' acc in
          self#constr "Pwith_type" [ a; b ]
        | Pwith_module (a, b), Pwith_module (a', b') ->
          let a = self#longident_loc a a' acc in
          let b = self#longident_loc b b' acc in
          self#constr "Pwith_module" [ a; b ]
        | Pwith_typesubst (a, b), Pwith_typesubst (a', b') ->
          let a = self#longident_loc a a' acc in
          let b = self#type_declaration b b' acc in
          self#constr "Pwith_typesubst" [ a; b ]
        | Pwith_modsubst (a, b), Pwith_modsubst (a', b') ->
          let a = self#longident_loc a a' acc in
          let b = self#longident_loc b b' acc in
          self#constr "Pwith_modsubst" [ a; b ]
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "with_constraint" (show_with_constraint x)
              (show_with_constraint x') acc
          in
          acc

    method module_expr : module_expr -> module_expr -> json =
      fun { pmod_desc; pmod_loc; pmod_attributes }
          { pmod_desc = pmod_desc'
          ; pmod_loc = pmod_loc'
          ; pmod_attributes = pmod_attributes'
          } ->
        let acc = self#module_expr_desc pmod_desc pmod_desc' acc in
        let acc = self#location pmod_loc pmod_loc' acc in
        let acc = self#attributes pmod_attributes pmod_attributes' acc in
        acc

    method module_expr_desc : module_expr_desc -> module_expr_desc -> json =
      fun x x' ->
        match (x, x') with
        | Pmod_ident a, Pmod_ident a' -> self#longident_loc a a' acc
        | Pmod_structure a, Pmod_structure a' -> self#structure a a' acc
        | Pmod_functor (a, b), Pmod_functor (a', b') ->
          let a = self#functor_parameter a a' acc in
          let b = self#module_expr b b' acc in
          self#constr "Pmod_functor" [ a; b ]
        | Pmod_apply (a, b), Pmod_apply (a', b') ->
          let a = self#module_expr a a' acc in
          let b = self#module_expr b b' acc in
          self#constr "Pmod_apply" [ a; b ]
        | Pmod_constraint (a, b), Pmod_constraint (a', b') ->
          let a = self#module_expr a a' acc in
          let b = self#module_type b b' acc in
          self#constr "Pmod_constraint" [ a; b ]
        | Pmod_unpack a, Pmod_unpack a' -> self#expression a a' acc
        | Pmod_extension a, Pmod_extension a' -> self#extension a a' acc
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "module_expr_desc" (show_module_expr_desc x)
              (show_module_expr_desc x') acc
          in
          acc

    method structure : structure -> structure -> json =
      self#list self#structure_item

    method structure_item : structure_item -> structure_item -> json =
      fun { pstr_desc; pstr_loc }
          { pstr_desc = pstr_desc'; pstr_loc = pstr_loc' } ->
        let acc = self#structure_item_desc pstr_desc pstr_desc' acc in
        let acc = self#location pstr_loc pstr_loc' acc in
        acc |> Base.List.dedup_and_sort ~compare:compare_diff

    method structure_item_desc
        : structure_item_desc -> structure_item_desc -> json =
      fun x x' ->
        match (x, x') with
        | Pstr_eval (a, b), Pstr_eval (a', b') ->
          let a = self#expression a a' acc in
          let b = self#attributes b b' acc in
          self#constr "Pstr_eval" [ a; b ]
        | Pstr_value (a, b), Pstr_value (a', b') ->
          let a = self#rec_flag a a' acc in
          let b = self#list self#value_binding b b' acc in
          self#constr "Pstr_value" [ a; b ]
        | Pstr_primitive a, Pstr_primitive a' -> self#value_description a a' acc
        | Pstr_type (a, b), Pstr_type (a', b') ->
          let a = self#rec_flag a a' acc in
          let b = self#list self#type_declaration b b' acc in
          self#constr "Pstr_type" [ a; b ]
        | Pstr_typext a, Pstr_typext a' -> self#type_extension a a' acc
        | Pstr_exception a, Pstr_exception a' -> self#type_exception a a' acc
        | Pstr_module a, Pstr_module a' -> self#module_binding a a' acc
        | Pstr_recmodule a, Pstr_recmodule a' ->
          self#list self#module_binding a a' acc
        | Pstr_modtype a, Pstr_modtype a' ->
          self#module_type_declaration a a' acc
        | Pstr_open a, Pstr_open a' -> self#open_declaration a a' acc
        | Pstr_class a, Pstr_class a' ->
          self#list self#class_declaration a a' acc
        | Pstr_class_type a, Pstr_class_type a' ->
          self#list self#class_type_declaration a a' acc
        | Pstr_include a, Pstr_include a' -> self#include_declaration a a' acc
        | Pstr_attribute a, Pstr_attribute a' -> self#attribute a a' acc
        | Pstr_extension (a, b), Pstr_extension (a', b') ->
          let a = self#extension a a' acc in
          let b = self#attributes b b' acc in
          self#constr "Pstr_extension" [ a; b ]
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "structure_item_desc"
              (show_structure_item_desc x)
              (show_structure_item_desc x')
              acc
          in
          acc

    method value_binding : value_binding -> value_binding -> json =
      fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }
          { pvb_pat = pvb_pat'
          ; pvb_expr = pvb_expr'
          ; pvb_attributes = pvb_attributes'
          ; pvb_loc = pvb_loc'
          } ->
        let acc = self#pattern pvb_pat pvb_pat' acc in
        let acc = self#expression pvb_expr pvb_expr' acc in
        let acc = self#attributes pvb_attributes pvb_attributes' acc in
        let acc = self#location pvb_loc pvb_loc' acc in
        acc

    method module_binding : module_binding -> module_binding -> json =
      fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc }
          { pmb_name = pmb_name'
          ; pmb_expr = pmb_expr'
          ; pmb_attributes = pmb_attributes'
          ; pmb_loc = pmb_loc'
          } ->
        let acc = self#loc (self#option self#string) pmb_name pmb_name' acc in
        let acc = self#module_expr pmb_expr pmb_expr' acc in
        let acc = self#attributes pmb_attributes pmb_attributes' acc in
        let acc = self#location pmb_loc pmb_loc' acc in
        acc

    method toplevel_phrase : toplevel_phrase -> toplevel_phrase -> json =
      fun x x' ->
        match (x, x') with
        | Ptop_def a, Ptop_def a' -> self#structure a a' acc
        | Ptop_dir a, Ptop_dir a' -> self#toplevel_directive a a' acc
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "toplevel_phrase" (show_toplevel_phrase x)
              (show_toplevel_phrase x') acc
          in
          acc

    method toplevel_directive : toplevel_directive -> toplevel_directive -> json
        =
      fun { pdir_name; pdir_arg; pdir_loc }
          { pdir_name = pdir_name'; pdir_arg = pdir_arg'; pdir_loc = pdir_loc' } ->
        let acc = self#loc self#string pdir_name pdir_name' acc in
        let acc = self#option self#directive_argument pdir_arg pdir_arg' acc in
        let acc = self#location pdir_loc pdir_loc' acc in
        acc

    method directive_argument : directive_argument -> directive_argument -> json
        =
      fun { pdira_desc; pdira_loc }
          { pdira_desc = pdira_desc'; pdira_loc = pdira_loc' } ->
        let acc = self#directive_argument_desc pdira_desc pdira_desc' acc in
        let acc = self#location pdira_loc pdira_loc' acc in
        acc

    method directive_argument_desc
        : directive_argument_desc -> directive_argument_desc -> json =
      fun x x' ->
        match (x, x') with
        | Pdir_string a, Pdir_string a' -> self#string a a' acc
        | Pdir_int (a, b), Pdir_int (a', b') ->
          let a = self#string a a' acc in
          let b = self#option self#char b b' acc in
          self#constr "Pdir_int" [ a; b ]
        | Pdir_ident a, Pdir_ident a' -> self#longident a a' acc
        | Pdir_bool a, Pdir_bool a' -> self#bool a a' acc
        | _ ->
          diff_count <- diff_count + 1;
          let acc =
            add_diff "directive_argument_desc"
              (show_directive_argument_desc x)
              (show_directive_argument_desc x')
              acc
          in
          acc

    method cases : cases -> cases -> json = self#list self#case
  end

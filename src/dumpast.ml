open Ppxlib
open Jsonoo.Encode

let stringify j =
  let str = Jsonoo.stringify j in
  String.sub str 1 (String.length str - 2)

let parse_ast =
  object (self)
    inherit [Jsonoo.t] Ast_traverse.lift as super

    method unit () = null

    method tuple args = list id args

    method string value = string value

    method float value = float value

    method bool value = bool value

    method record args = object_ args

    method other _ = string "other?"

    method nativeint _ = string "nativeint"

    method int64 _ = string "int64"

    method int32 _ = string "int32"

    method int value = int value

    method constr label args = object_ [ (label, list id args) ]

    method char value = char value

    method array f arg = list id (Array.to_list arg |> List.map f)

    method! value_binding { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } =
      object_
        [ ("type", string "value_binding")
        ; ("pvb_pat", super#pattern pvb_pat)
        ; ("pvb_expr", self#expression pvb_expr)
        ; ("pvb_attributes", super#attributes pvb_attributes)
        ; ("pvb_loc", super#location pvb_loc)
        ]

    method! structure_item { pstr_desc; pstr_loc } =
      object_
        [ ("type", string "structure_item")
        ; ("pstr_desc", self#structure_item_desc pstr_desc)
        ; ("pstr_loc", super#location pstr_loc)
        ]

    method! structure s = object_ [ ("structure", list self#structure_item s) ]

    method! longident lident =
      match lident with
      | Lident txt -> string ("Lident \"" ^ txt ^ "\"")
      | Ldot (t, s) ->
        string ("Ldot (" ^ stringify (self#longident t) ^ "," ^ "\"" ^ s ^ "\")")
      | Lapply (t, k) ->
        string
          ( "Lapply ("
          ^ stringify (self#longident t)
          ^ ","
          ^ stringify (self#longident k)
          ^ ")" )

    method! longident_loc { txt; _ } = self#longident txt

    method! expression_desc expression_desc =
      match expression_desc with
      | Pexp_ident txt ->
        string ("Pexp_ident {" ^ stringify (self#longident_loc txt) ^ "}")
      | _ -> super#expression_desc expression_desc

    method! expression { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes }
        =
      object_
        [ ("type", string "expression")
        ; ("pexp_desc", self#expression_desc pexp_desc)
        ; ("pexp_loc", super#location pexp_loc)
        ; ("pexp_loc_stack", super#location_stack pexp_loc_stack)
        ; ("pexp_attributes", super#attributes pexp_attributes)
        ]

    method! list f = list f
  end

let transform source =
  try
    let v = Parse.implementation (Lexing.from_string source) in
    parse_ast#structure v
  with _ -> Jsonoo.Encode.string "Syntax error"

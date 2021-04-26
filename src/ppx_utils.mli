open Import
open Ppxlib


val get_pp_path : document:TextDocument.t -> string

val get_preprocessed_structure : string -> structure

(* val ocamlformat : string -> string
 *)
val project_root_path : document:TextDocument.t -> string

val relative_document_path : document:TextDocument.t -> string


val get_pp_pp_structure : document:TextDocument.t -> string
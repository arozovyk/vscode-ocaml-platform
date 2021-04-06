open Import
open Ppxlib

val pp_exists : string -> bool

val get_pp_path : document:TextDocument.t -> string

val get_preprocessed_structure : string -> structure

val ocamlformat : string -> string

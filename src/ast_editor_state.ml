open Import

type t =
  { (* The webview store that maps string value of Uri.t of the document to the
       related webview. *)
    mutable webview_map : (string, WebView.t, String.comparator_witness) Map.t
        (* Indicates current mode inside AST explorer (true = Original, false =
           Prerocessed) *)
  ; mutable original_mode : bool
        (* Contains Some value when hover mode is enabled, None otherwise *)
  ; mutable hover_disposable : Disposable.t option
        (* Mapping between string value of Uri.t of the original (non
           preprocessed) document to boolean, indicating that the document has
           changed since opening its Preprocessed Document *)
  ; mutable pp_doc_to_changed_origin_map :
      (string, bool, String.comparator_witness) Map.t
        (* Mapping beween the original and Preprocessed Document used to
           simultaneously communicate with a common webview. *)
  ; mutable origin_to_pp_doc_map :
      (string, string, String.comparator_witness) Map.t
  }

let make () =
  let webview_map = Map.empty (module String) in
  let original_mode = true in
  let hover_disposable = None in
  let pp_doc_to_changed_origin_map = Map.empty (module String) in
  let origin_to_pp_doc_map = Map.empty (module String) in
  { webview_map
  ; original_mode
  ; hover_disposable
  ; pp_doc_to_changed_origin_map
  ; origin_to_pp_doc_map
  }

let find_original_doc_by_pp_uri t ~uri = Map.find t.origin_to_pp_doc_map uri

let find_webview_by_doc t ~document =
  let doc_uri = Uri.toString (TextDocument.uri document) () in
  match Map.find t.webview_map doc_uri with
  | wv_opt when t.original_mode -> wv_opt
  | Some _ -> None
  | None -> (
    let open Option.O in
    let* key = find_original_doc_by_pp_uri ~uri_string:doc_uri t in
    match Map.find t.webview_map key with
    | wv_opt when not t.original_mode -> wv_opt
    | _ -> None)

let set_changes_tracking t origin pp_doc =
  let origin_uri = Uri.toString (TextDocument.uri origin) () in
  let pp_doc_uri = Uri.toString (TextDocument.uri pp_doc) () in
  t.origin_to_pp_doc_map <-
    Map.set t.origin_to_pp_doc_map ~key:origin_uri ~data:pp_doc_uri;
  t.pp_doc_to_changed_origin_map <-
    Map.set t.pp_doc_to_changed_origin_map ~key:pp_doc_uri ~data:false

let get_original_mode t = t.original_mode

let set_original_mode t value = t.original_mode <- value

let get_hover_disposable t = t.hover_disposable

let set_hover_disposable t hover_disposable =
  t.hover_disposable <- hover_disposable

let set_origin_changed t ~data ~key =
  t.pp_doc_to_changed_origin_map <-
    Map.set t.pp_doc_to_changed_origin_map ~key ~data

let entry_exists t ~origin_doc ~pp_doc =
  Map.existsi t.origin_to_pp_doc_map ~f:(fun ~key ~data ->
      String.equal pp_doc data && String.equal origin_doc key)

let on_origin_update_content t changed_document =
  match
    Map.find t.origin_to_pp_doc_map
      (Uri.toString (TextDocument.uri changed_document) ())
  with
  | Some key -> set_origin_changed t ~key ~data:true
  | None -> ()

let get_pp_doc_to_changed_origin_map t = t.pp_doc_to_changed_origin_map

let remove_doc_entries (t : t) uri =
  let pp_doc_to_changed_origin_map, origin_to_pp_doc_map =
    let origin_uri = Uri.toString (TextDocument.uri uri) () in
    match Map.find t.origin_to_pp_doc_map origin_uri with
    | Some uri ->
      ( Map.remove t.pp_doc_to_changed_origin_map uri
      , Map.remove t.origin_to_pp_doc_map origin_uri )
    | None ->
      ( Map.remove t.pp_doc_to_changed_origin_map origin_uri
      , Map.filteri t.origin_to_pp_doc_map ~f:(fun ~key:_ ~data ->
            not (String.equal data origin_uri)) )
  in
  t.pp_doc_to_changed_origin_map <- pp_doc_to_changed_origin_map;
  t.origin_to_pp_doc_map <- origin_to_pp_doc_map

let set_webview t ~uri webview =
  t.webview_map <- Map.set ~key:uri ~data:webview t.webview_map

let pp_status t ~uri =
  match Map.find t.pp_doc_to_changed_origin_map uri with
  | Some true -> `Original
  | Some false
  | None ->
    `Absent_or_pped

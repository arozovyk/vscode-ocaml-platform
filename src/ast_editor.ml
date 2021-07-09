open Import
open Ppx_utils

(*Indicates the output mode in order to highlight the right document*)

let original_mode = ref true

let webview_map = ref (Map.empty (module String))

let doc_string_uri ~document = Uri.toString (TextDocument.uri document) ()

let pp_doc_to_changed_origin_map = ref (Map.empty (module String))

let origin_to_pp_doc_map = ref (Map.empty (module String))

let entry_exists k d =
  Map.existsi !origin_to_pp_doc_map ~f:(fun ~key ~data ->
      String.equal d data && String.equal k key)

let remove_keys_by_data map d =
  map := Map.filteri ~f:(fun ~key:_ ~data -> not (String.equal data d)) !map

let put_keys_into_a_list data_serached map =
  Map.filteri ~f:(fun ~key:_ ~data -> String.equal data_serached data) map
  |> Map.to_alist |> List.unzip |> fst

let set_origin_changed ~data ~key =
  pp_doc_to_changed_origin_map :=
    Map.set !pp_doc_to_changed_origin_map ~key ~data

let set_changes_tracking origin pp_doc =
  origin_to_pp_doc_map :=
    Map.set !origin_to_pp_doc_map
      ~key:(doc_string_uri ~document:origin)
      ~data:(doc_string_uri ~document:pp_doc);
  pp_doc_to_changed_origin_map :=
    Map.set
      !pp_doc_to_changed_origin_map
      ~key:(doc_string_uri ~document:pp_doc)
      ~data:false

let on_origin_update_content changed_document =
  match
    Map.find !origin_to_pp_doc_map (doc_string_uri ~document:changed_document)
  with
  | Some key -> set_origin_changed ~key ~data:true
  | None -> ()

(*for debugging purposes*)
(* let _print_state () = print_endline "State: "; Map.iteri
   !pp_doc_to_changed_origin_map ~f:(fun ~key ~data -> print_string ("(" ^ key ^
   "," ^ Bool.to_string data ^ ")")); print_endline ""; Map.iteri
   !origin_to_pp_doc_map ~f:(fun ~key ~data -> print_string ("(" ^ key ^ "," ^
   data ^ ")")); print_endline "" *)

let send_msg t value ~(webview : WebView.t) =
  let msg = Ojs.empty_obj () in
  Ojs.set_prop_ascii msg "type" (Ojs.string_to_js t);
  Ojs.set_prop_ascii msg "value" value;
  let _ = WebView.postMessage webview msg in
  ()

let document_eq a b =
  String.equal
    (Uri.toString (TextDocument.uri a) ())
    (Uri.toString (TextDocument.uri b) ())

let get_html_for_WebView_from_file () =
  let filename = Node.__dirname () ^ "/../astexplorer/dist/index.html" in
  Fs.readFile filename

(* let write_to_file content path = let tmp_path = path in let oc =
   Out_channel.create tmp_path in Out_channel.fprintf oc "%s\n" content;
   Out_channel.close oc *)

let transform_to_ast ~(document : TextDocument.t) ~(webview : WebView.t) =
  let open Jsonoo.Encode in
  let origin_json = TextDocument.getText document () |> Dumpast.transform in
  let _pp_path = get_pp_path ~document in
  let pp_value =
    try
      (*The following would return the structure with ghost_locs*)
      let ppml_structure = get_preprocessed_structure (get_pp_path ~document) in
      (* let ppml_json = Dumpast.from_structure ppml_structure in *)
      let pp_code = get_pp_pp_structure ~document in
      let reparsed_structure =
        pp_code |> Lexing.from_string |> Parse.implementation
      in
      (* print_endline "before reparsing"; write_to_file (Jsonoo.stringify
         ppml_json) "/tmp/ppml1"; write_to_file (Jsonoo.stringify
         (Dumpast.transform pp_code)) "/tmp/reparsed2"; *)
      let reparsed_json = Dumpast.reparse ppml_structure reparsed_structure in
      (* write_to_file (Jsonoo.stringify reparsed_json) "/tmp/reparsed_json"; *)
      reparsed_json
      (* Use only the actual locations while waiting on the bug fix where
         reparsing returns two different ASTs, most likely due to the AST
         version difference (resulting from the use of OMP with Pprintast ?) *)
    with
    | _ -> (* print_endline (Exn.to_string e); *) null
  in
  let astpair = object_ [ ("ast", origin_json); ("pp_ast", pp_value) ] in
  send_msg "parse" (Jsonoo.t_to_js astpair) ~webview

let replace_document_content ~document ~content =
  let first_line = TextDocument.lineAt document ~line:0 in
  let last_line =
    TextDocument.lineAt document ~line:(TextDocument.lineCount document - 1)
  in
  let range =
    Range.makePositions
      ~start:(TextLine.range first_line |> Range.start)
      ~end_:(TextLine.range last_line |> Range.end_)
  in
  let edit = WorkspaceEdit.make () in
  WorkspaceEdit.replace edit
    ~uri:(TextDocument.uri document)
    ~range ~newText:content;
  Workspace.applyEdit ~edit

let open_pp_doc ~document =
  let open Promise.Syntax in
  let pp_pp_str = get_pp_pp_structure ~document in
  let* doc =
    Workspace.openTextDocument
      (`Uri
        (Uri.parse
           ("post-ppx: "
           ^ TextDocument.fileName document
           ^ "?" ^ "" (* pp_pp_str *))
           ()))
  in
  (*save uri pair to track changes*)
  set_changes_tracking document doc;
  let _ =
    let+ _ = replace_document_content ~content:pp_pp_str ~document:doc in
    ()
  in
  let+ _ =
    Window.showTextDocument ~document:(`TextDocument doc)
      ~column:ViewColumn.Beside ()
  in
  0

let reload_pp_doc ~document =
  let open Promise.Syntax in
  let visibleTextEditors = Window.visibleTextEditors () in
  let origin_uri =
    match
      put_keys_into_a_list (doc_string_uri ~document) !origin_to_pp_doc_map
      (*FIXME: this can fail because origin_to_pp_doc_map is not cleaned on
        closed document *)
    with
    | x :: _ -> x
    | _ -> failwith "Failed finding the original document URI."
  in
  let* original_document =
    Workspace.openTextDocument (`Uri (Uri.parse origin_uri ()))
  in
  match
    List.find
      ~f:(fun editor -> document_eq (TextEditor.document editor) document)
      visibleTextEditors
  with
  | Some _ ->
    let+ _ =
      set_origin_changed ~key:(doc_string_uri ~document) ~data:false;
      replace_document_content
        ~content:(get_pp_pp_structure ~document:original_document)
        ~document
    in
    0
  | None -> Promise.resolve 1

let manage_choice choice ~document : int Promise.t =
  let path = Path.of_string (project_root_path ~document) in
  let buildCmd () =
    Cmd.run ~cwd:path (Cmd.Shell "eval $(opam env); dune build")
  in
  let rec build_project () =
    let open Promise.Syntax in
    let* res = buildCmd () in
    if res.exitCode = 0 then
      (* FIXME: remove entries on document close *)
      if
        Map.existsi !pp_doc_to_changed_origin_map ~f:(fun ~key ~data:_ ->
            String.equal key (doc_string_uri ~document))
      then
        reload_pp_doc ~document
      else
        open_pp_doc ~document
    else
      let* perror =
        Window.showErrorMessage
          ~message:"Building project failed, fix project errors and retry."
          ~choices:[ ("Retry running `dune build`", 0); ("Abandon", 1) ]
          ()
      in
      match perror with
      | Some 0 -> build_project ()
      | _ -> Promise.resolve 1
  in
  match choice with
  | Some 0 -> build_project ()
  | _ -> Promise.resolve 1

let open_ast_explorer ~uri =
  let _ =
    Vscode.Commands.executeCommand ~command:"vscode.openWith"
      ~args:
        [ Uri.t_to_js uri
        ; Ojs.string_to_js "ast-editor"
        ; ViewColumn.t_to_js ViewColumn.Beside
        ]
  in
  ()

let manage_open_failure ~document =
  let open Promise.Syntax in
  let* choice =
    Window.showInformationMessage
      ~message:
        ("Seems like the file '"
        ^ relative_document_path ~document
        ^ "' haven't been preprocessed yet.")
      ~choices:[ ("Run `dune build`", 0); ("Abandon", 1) ]
      ()
  in
  manage_choice choice ~document

(*this needs testing (especially on linux)*)
let open_preprocessed_doc_to_the_side ~document =
  try open_pp_doc ~document with
  | _ -> manage_open_failure ~document

let open_both_ppx_ast ~document =
  let open Promise.Syntax in
  let+ pp_doc_open = open_preprocessed_doc_to_the_side ~document in
  if pp_doc_open = 0 then
    open_ast_explorer ~uri:(TextDocument.uri document)
  else
    ()

let manage_changed_origin ~document =
  let open Promise.Syntax in
  let* choice =
    Window.showInformationMessage
      ~message:
        "The original document have been changed, would you like to rebuild \
         the project?"
      ~choices:[ ("Run `dune build`", 0); ("Cancel", 1) ]
      ()
  in
  manage_choice choice ~document

module Command = struct
  let _reveal_ast_node =
    let handler _ ~textEditor ~edit:_ ~args:_ =
      let (_ : unit Promise.t) =
        let selection = Vscode.TextEditor.selection textEditor in
        let document = TextEditor.document textEditor in
        let position = Vscode.Selection.start selection in
        let webview_opt =
          match Map.find !webview_map (doc_string_uri ~document) with
          | wv_opt when !original_mode -> wv_opt
          | None -> (
            match
              put_keys_into_a_list (doc_string_uri ~document)
                !origin_to_pp_doc_map
            with
            | [ k ] -> (
              match Map.find !webview_map k with
              | wv_opt when not !original_mode -> wv_opt
              | _ -> None)
            | _ -> None)
          | _ -> None
        in
        let offset = TextDocument.offsetAt document ~position in
        Promise.make (fun ~resolve:_ ~reject:_ ->
            match webview_opt with
            | Some webview -> send_msg "focus" (Ojs.int_to_js offset) ~webview
            | None ->
              let _ =
                Window.showErrorMessage
                  ~message:
                    "Wrong outputmode inside the ASTexplorer, please select \
                     the correct tab"
                  ~choices:[ ("Close", 0) ] ()
              in
              ())
      in
      ()
    in
    Extension_commands.register_text_editor
      ~id:Extension_consts.Commands.reveal_ast_node handler

  let _open_ast_explorer_to_the_side =
    let handler _ ~textEditor ~edit:_ ~args:_ =
      let (_ : unit Promise.t) =
        let uri = TextEditor.document textEditor |> TextDocument.uri in
        Promise.make (fun ~resolve:_ ~reject:_ -> open_ast_explorer ~uri)
      in
      ()
    in
    Extension_commands.register_text_editor
      ~id:Extension_consts.Commands.open_ast_explorer_to_the_side handler

  let _show_preprocessed_document =
    let handler _ ~textEditor ~edit:_ ~args:_ =
      let document = TextEditor.document textEditor in
      let (_ : unit Promise.t) =
        Promise.make (fun ~resolve:_ ~reject:_ ->
            let _ = open_preprocessed_doc_to_the_side ~document in
            ())
      in
      ()
    in
    Extension_commands.register_text_editor
      ~id:Extension_consts.Commands.show_preprocessed_document handler

  let _open_pp_editor_and_ast_explorer =
    let handler _ ~textEditor ~edit:_ ~args:_ =
      let (_ : unit Promise.t) =
        let document = TextEditor.document textEditor in
        Promise.make (fun ~resolve:_ ~reject:_ ->
            let _ = open_both_ppx_ast ~document in
            ())
      in
      ()
    in
    Extension_commands.register_text_editor
      ~id:Extension_consts.Commands.open_pp_editor_and_ast_explorer handler
end

let _on_hover custom_doc webview =
  let hover =
    Hover.make ~contents:(`MarkdownString (MarkdownString.make ~value:"" ()))
  in
  let provideHover ~(document : TextDocument.t) ~(position : Position.t)
      ~token:_ =
    let offset = TextDocument.offsetAt document ~position in
    if document_eq custom_doc document then
      send_msg "focus" (Ojs.int_to_js offset) ~webview
    else
      ();
    `Value (Some [ hover ])
  in
  let provider = HoverProvider.create ~provideHover in
  Vscode.Languages.registerHoverProvider ~selector:(`String "ocaml") ~provider

let close_visible_editors_by_uri uri =
  let f e =
    let visibleDocument = TextEditor.document e in
    let open Promise.Syntax in
    if String.equal uri (doc_string_uri ~document:visibleDocument) then
      let _ =
        let+ _ =
          Window.showTextDocument ~document:(`TextDocument visibleDocument) ()
        in
        Vscode.Commands.executeCommand
          ~command:"workbench.action.closeActiveEditor" ~args:[]
      in
      ()
    else
      ()
  in
  Window.visibleTextEditors () |> List.iter ~f

let text_document_content_provider_ppx =
  let module EventEmitter = EventEmitter.Make (Uri) in
  let event_emitter = EventEmitter.make () in
  let onDidChange = EventEmitter.event event_emitter in
  let provideTextDocumentContent ~uri ~token:_ : string ProviderResult.t =
    `Value (Some (Uri.query uri))
  in
  TextDocumentContentProvider.create ~provideTextDocumentContent ~onDidChange

let onDidChangeActiveTextEditor_listener e =
  if not (TextEditor.t_to_js e |> Ojs.is_null) then
    let document = TextEditor.document e in
    match Map.find !pp_doc_to_changed_origin_map (doc_string_uri ~document) with
    | Some true ->
      let _ = manage_changed_origin ~document in
      ()
    | _ -> ()
  else
    ()

let onDidCloseTextDocument_listener (document : TextDocument.t) =
  let origin_uri = doc_string_uri ~document in
  match Map.find !origin_to_pp_doc_map origin_uri with
  | Some uri ->
    pp_doc_to_changed_origin_map := Map.remove !pp_doc_to_changed_origin_map uri;
    origin_to_pp_doc_map := Map.remove !origin_to_pp_doc_map origin_uri;
    (* close corresponding pp buffers to avoid confusion *)
    close_visible_editors_by_uri uri
  | None ->
    remove_keys_by_data origin_to_pp_doc_map origin_uri;
    pp_doc_to_changed_origin_map :=
      Map.remove !pp_doc_to_changed_origin_map origin_uri

let onDidChangeTextDocument_listener event ~(document : TextDocument.t)
    ~(webview : WebView.t) =
  let changed_document = TextDocumentChangeEvent.document event in
  if document_eq document changed_document then
    transform_to_ast ~document ~webview
  else
    ()

let onDidSaveTextDocument_listener_pp document =
  on_origin_update_content document

(* let _ = o(* pen_pp_pp_pp_doc ~document in (); *) let ppstruct =
   get_preprocessed_structure (get_pp_path ~document) in let reparsed_json =
   get_pp_pp_structure ~document |> Lexing.from_string |> Parse.implementation
   in try let res = Dumpast.reparse ppstruct reparsed_json in print_endline
   "ok"; write_to_file (Jsonoo.stringify (Dumpast.from_structure ppstruct))
   "/tmp/origin_res"; write_to_file (Jsonoo.stringify res) "/tmp/result"; with |
   Traverse_ast2.Reparse_error e -> print_endline ("Error at " ^ e) | e ->
   print_endline (Exn.to_string e) *)

let onDidReceiveMessage_listener msg ~(document : TextDocument.t) =
  if Ojs.has_property msg "selectedOutput" then
    original_mode := not !original_mode
  else if Ojs.has_property msg "begin" && Ojs.has_property msg "end" then
    let cbegin =
      Int.of_string (Ojs.string_of_js (Ojs.get_prop_ascii msg "begin"))
    in
    let cend =
      Int.of_string (Ojs.string_of_js (Ojs.get_prop_ascii msg "end"))
    in
    let maybe_pair_editors =
      List.map (Vscode.Window.visibleTextEditors ()) ~f:(fun editor ->
          let visible_doc = TextEditor.document editor in
          if (* !original_mode && *) document_eq document visible_doc then
            (Some editor, None)
          else if
            (* (not !original_mode) && *)
            (entry_exists (doc_string_uri ~document))
              (doc_string_uri ~document:visible_doc)
          then
            (None, Some editor)
          else
            (None, None))
      |> List.filter ~f:(function
           | None, None -> false
           | _ -> true)
    in
    let apply_selection editor cbegin cend =
      let document = TextEditor.document editor in
      let anchor = Vscode.TextDocument.positionAt document ~offset:cbegin in
      let active = Vscode.TextDocument.positionAt document ~offset:cend in
      TextEditor.set_selection editor (Selection.makePositions ~anchor ~active);
      TextEditor.revealRange editor
        ~range:(Range.makePositions ~start:anchor ~end_:active)
        ();
      (*FIXME: not accessing editor after the combination of revealRange and
        set_selection (separately) results in a expetion being thrown*)
      let _ = TextEditor.selections editor in
      ()
    in
    List.iter
      ~f:(fun e ->
        match e with
        | Some editor, None -> apply_selection editor cbegin cend
        | None, Some editor
          when Ojs.has_property msg "r_begin"
               && Ojs.has_property msg "r_end"
               && not !original_mode ->
          let rcbegin =
            Int.of_string (Ojs.string_of_js (Ojs.get_prop_ascii msg "r_begin"))
          in
          let rcend =
            Int.of_string (Ojs.string_of_js (Ojs.get_prop_ascii msg "r_end"))
          in
          apply_selection editor rcbegin rcend
        | _ -> ())
      maybe_pair_editors
  else
    ()

let resolveCustomTextEditor ~(document : TextDocument.t) ~webviewPanel ~token:_ :
    CustomTextEditorProvider.ResolvedEditor.t =
  let webview = WebviewPanel.webview webviewPanel in
  (*persist the webview*)
  webview_map :=
    Map.set !webview_map ~key:(doc_string_uri ~document) ~data:webview;
  let options = WebView.options webview in
  WebviewOptions.set_enableScripts options true;
  WebView.set_options webview options;
  let onDidReceiveMessage_disposable =
    WebView.onDidReceiveMessage webview
      ~listener:(onDidReceiveMessage_listener ~document)
      ()
  in

  let _ =
    let open Promise.Syntax in
    let+ r = get_html_for_WebView_from_file () in
    WebView.set_html webview r
  in
  let onDidChangeTextDocument_disposable =
    Workspace.onDidChangeTextDocument
      ~listener:(onDidChangeTextDocument_listener ~webview ~document)
      ()
  in
  let _ =
    WebviewPanel.onDidDispose webviewPanel
      ~listener:(fun () ->
        original_mode := true;
        Disposable.dispose onDidReceiveMessage_disposable;
        Disposable.dispose onDidChangeTextDocument_disposable)
      ()
  in
  transform_to_ast ~document ~webview;
  (* let _ = _on_hover document webview in *)
  CustomTextEditorProvider.ResolvedEditor.t_of_js (Ojs.variable "null")

let register extension =
  let editorProvider =
    `CustomEditorProvider
      (CustomTextEditorProvider.create ~resolveCustomTextEditor)
  in
  let _ =
    Window.onDidChangeActiveTextEditor ()
      ~listener:onDidChangeActiveTextEditor_listener ()
  in
  let disposable =
    Workspace.onDidSaveTextDocument ~listener:onDidSaveTextDocument_listener_pp
      ()
  in
  Vscode.ExtensionContext.subscribe extension ~disposable;
  let disposable =
    Workspace.onDidCloseTextDocument ~listener:onDidCloseTextDocument_listener
      ()
  in
  Vscode.ExtensionContext.subscribe extension ~disposable;
  let disposable =
    Vscode.Window.registerCustomEditorProvider ~viewType:"ast-editor"
      ~provider:editorProvider
  in
  Vscode.ExtensionContext.subscribe extension ~disposable;
  let disposable =
    Vscode.Workspace.registerTextDocumentContentProvider ~scheme:"post-ppx"
      ~provider:text_document_content_provider_ppx
  in
  Vscode.ExtensionContext.subscribe extension ~disposable

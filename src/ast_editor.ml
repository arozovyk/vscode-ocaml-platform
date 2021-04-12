open Import
open Ppx_utils
module StringMap = Map.Make (String)

(*TODO: decreaste the number on disposed webviews, and replace the function with
  proper webview store mechanism*)
let number_of_visible_webviews = ref 0

let pp_doc_to_changed_origin_map = ref StringMap.empty

let origin_to_pp_doc_map = ref StringMap.empty

let doc_string_uri ~document = Uri.toString (TextDocument.uri document) ()

let put_keys_into_a_list data_serached map =
  StringMap.filteri ~f:(fun ~key:_ ~data -> String.equal data_serached data) map
  |> StringMap.to_alist |> List.unzip |> fst

let set_changes_tracking origin pp_doc =
  origin_to_pp_doc_map :=
    StringMap.set !origin_to_pp_doc_map
      ~key:(doc_string_uri ~document:origin)
      ~data:(doc_string_uri ~document:pp_doc);
  pp_doc_to_changed_origin_map :=
    StringMap.set
      !pp_doc_to_changed_origin_map
      ~key:(doc_string_uri ~document:pp_doc)
      ~data:false

let set_origin_changed ~data ~key =
  pp_doc_to_changed_origin_map :=
    StringMap.set !pp_doc_to_changed_origin_map ~key ~data

let on_origin_update_content changed_document =
  match
    StringMap.find !origin_to_pp_doc_map
      (doc_string_uri ~document:changed_document)
  with
  | Some key -> set_origin_changed ~key ~data:true
  | None -> ()

(*doesnt work because of the bug where the Webview doesn't increment the
  totalColumnCount*)
let _rightmost_column () =
  let open ViewColumn in
  let to_int col = t_to_js col |> Ojs.int_of_js in
  let of_int i = Ojs.int_to_js i |> t_of_js in
  let visibleTextEditors = Window.visibleTextEditors () in
  let iCol =
    ( List.fold visibleTextEditors ~init:One ~f:(fun acc editor ->
          let editorColumn =
            match TextEditor.viewColumn editor with
            | Some column -> column
            | None -> One
          in
          max (to_int acc) (to_int editorColumn) |> of_int)
    |> to_int )
    + 1
    + !number_of_visible_webviews
  in
  of_int iCol

let webview_map = ref StringMap.empty

let send_msg t value ~(webview : WebView.t) =
  let msg = Ojs.empty_obj () in
  Ojs.set msg "type" (Ojs.string_to_js t);
  Ojs.set msg "value" value;
  let _ = WebView.postMessage webview msg in
  ()

let document_eq a b =
  String.equal
    (Uri.toString (TextDocument.uri a) ())
    (Uri.toString (TextDocument.uri b) ())

let document_id document = Uri.toString (TextDocument.uri document) ()

let get_html_for_WebView_from_file () =
  let filename = Node.__dirname () ^ "/../astexplorer/dist/index.html" in
  In_channel.read_all filename

let transform_to_ast ~(document : TextDocument.t) ~(webview : WebView.t) =
  let open Jsonoo.Encode in
  let value = TextDocument.getText document () |> Dumpast.transform in
  let pp_path = get_pp_path ~document in
  let pp_value =
    if pp_exists pp_path then
      let ppstruct = get_preprocessed_structure (get_pp_path ~document) in
      Dumpast.from_structure ppstruct
    else
      null
  in
  let astpair = object_ [ ("ast", value); ("pp_ast", pp_value) ] in
  send_msg "parse" (Jsonoo.t_to_js astpair) ~webview

let savedWebviewPanels = ref []

let to_str col = ViewColumn.t_to_js col |> Ojs.int_of_js |> Int.to_string

let updateSavedWebViews ~(document : TextDocument.t) =
  List.iter !savedWebviewPanels ~f:(fun wvp ->
      let webview = WebviewPanel.webview !wvp in
      transform_to_ast ~document ~webview)

let _showNewWebviewPanel ~document () =
  let webviewpanel =
    Window.createWebviewPanel ~viewType:"ocaml" ~title:"Ast explorer"
      ~showOptions:(_rightmost_column ())
  in
  let webview = WebviewPanel.webview webviewpanel in
  WebView.set_html webview (get_html_for_WebView_from_file ());
  let options = WebView.options webview in
  WebviewOptions.set_enableScripts options true;
  WebView.set_options webview options;
  savedWebviewPanels := ref webviewpanel :: !savedWebviewPanels;
  transform_to_ast ~document ~webview;
  let listener () =
    number_of_visible_webviews := !number_of_visible_webviews - 1
  in
  let _ = WebviewPanel.onDidDispose webviewpanel ~listener () in
  let column = _rightmost_column () in
  print_endline ("Column webview  : " ^ to_str column);
  WebviewPanel.reveal webviewpanel ~preserveFocus:true ();
  number_of_visible_webviews := !number_of_visible_webviews + 1

let onDidChangeTextDocument_listener event ~(document : TextDocument.t)
    ~(webview : WebView.t) =
  let changed_document = TextDocumentChangeEvent.document event in
  if document_eq document changed_document then (
    transform_to_ast ~document ~webview;
    updateSavedWebViews ~document
  ) else
    ()

let onDidSaveTextDocument_listener_pp document =
  on_origin_update_content document

let onDidReceiveMessage_listener msg ~(document : TextDocument.t) =
  let cbegin = Int.of_string (Ojs.string_of_js (Ojs.get msg "begin")) in
  let cend = Int.of_string (Ojs.string_of_js (Ojs.get msg "end")) in
  let visibleTextEditors =
    List.filter (Vscode.Window.visibleTextEditors ()) ~f:(fun editor ->
        document_eq (TextEditor.document editor) document)
  in
  let apply_selection editor =
    let anchor = Vscode.TextDocument.positionAt document ~offset:cbegin in
    let active = Vscode.TextDocument.positionAt document ~offset:cend in
    TextEditor.revealRange editor
      ~range:(Range.makePositions ~start:anchor ~end_:active)
      ();
    TextEditor.set_selection editor (Selection.makePositions ~anchor ~active)
  in
  List.iter ~f:apply_selection visibleTextEditors

let get_pp_pp_structure ~document =
  let str = get_preprocessed_structure (get_pp_path ~document) in
  Format.asprintf "%a" Pprintast.structure str |> ocamlformat

let open_pp_doc ~document =
  let open Promise.Syntax in
  let* doc =
    let textDocumentOptions =
      let open Workspace in
      { language = "ocaml"; content = get_pp_pp_structure ~document }
    in
    Workspace.openTextDocument (`Interactive (Some textDocumentOptions))
  in
  (*save uri pair to track changes*)
  set_changes_tracking document doc;
  let+ _ =
    Window.showTextDocument ~document:(`TextDocument doc)
      ~column:ViewColumn.Beside ()
  in
  0

let reload_pp_doc ~document =
  let open Promise.Syntax in
  let visibleTextEditors = Window.visibleTextEditors () in
  let firstLine = TextDocument.lineAt ~line:0 document in
  let lastLine =
    TextDocument.lineAt ~line:(TextDocument.lineCount document - 1) document
  in

  let range =
    Range.makePositions
      ~start:(TextLine.range firstLine |> Range.start)
      ~end_:(TextLine.range lastLine |> Range.start)
  in
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
  let edit = WorkspaceEdit.make () in
  WorkspaceEdit.replace edit
    ~uri:(TextDocument.uri document)
    ~range
    ~newText:(get_pp_pp_structure ~document:original_document);
  match
    List.find
      ~f:(fun editor -> document_eq (TextEditor.document editor) document)
      visibleTextEditors
  with
  | Some _ ->
    let+ _ =
      set_origin_changed ~key:(doc_string_uri ~document) ~data:false;
      Workspace.applyEdit ~edit
    in
    0
  | None -> Promise.resolve 1

let manage_choice choice ~document : int Promise.t =
  let make_p c = Promise.resolve c in
  let buildCmd () =
    Sys.command
      ("cd " ^ project_root_path ~document ^ ";eval $(opam env); dune build")
  in
  let rec build_project () =
    let open Promise.Syntax in
    if buildCmd () = 0 then
      (* FIXME: remove entries on document close *)
      if
        StringMap.existsi !pp_doc_to_changed_origin_map ~f:(fun ~key ~data:_ ->
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
      | _ -> make_p 1
  in
  match choice with
  | Some 0 -> build_project ()
  | _ -> make_p 1

module Command = struct
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
          ( "Seems like the file '"
          ^ relative_document_path ~document
          ^ "' haven't been preprocessed yet." )
        ~choices:[ ("Run `dune build`", 0); ("Abandon", 1) ]
        ()
    in
    manage_choice choice ~document

  (*this needs testing (especially on linux)*)
  let open_preprocessed_doc_to_the_side ~document =
    let pp_path = get_pp_path ~document in
    if pp_exists pp_path then
      open_pp_doc ~document
    else
      manage_open_failure ~document

  let open_both_ppx_ast ~document =
    let open Promise.Syntax in
    let+ pp_doc_open = open_preprocessed_doc_to_the_side ~document in
    if pp_doc_open = 0 then
      open_ast_explorer ~uri:(TextDocument.uri document)
    else
      ()

  let _reveal_ast_node =
    let handler _ ~textEditor ~edit:_ ~args:_ =
      let (_ : unit Promise.t) =
        let selection = Vscode.TextEditor.selection textEditor in
        let document = TextEditor.document textEditor in
        let position = Vscode.Selection.start selection in
        let webview =
          match StringMap.find !webview_map (document_id document) with
          | Some wv -> wv
          | None ->
            print_endline "Not found";
            failwith "Webview wasnt found"
        in
        let offset = TextDocument.offsetAt document ~position in
        Promise.make (fun ~resolve:_ ~reject:_ ->
            send_msg "focus" (Ojs.int_to_js offset) ~webview)
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
      let (_ : unit Promise.t) =
        let document = TextEditor.document textEditor in

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
    Hover.make
      ~contents:
        (`MarkdownString (MarkdownString.make ~value:"hover is working" ()))
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

let resolveCustomTextEditor ~(document : TextDocument.t) ~webviewPanel ~token :
    CustomTextEditorProvider.ResolvedEditor.t =
  let _ = token in
  let webview = WebviewPanel.webview webviewPanel in
  (*persist the webview*)
  webview_map :=
    StringMap.set !webview_map ~key:(document_id document) ~data:webview;
  let options = WebView.options webview in
  WebviewOptions.set_enableScripts options true;
  WebView.set_options webview options;
  let _ =
    WebView.onDidReceiveMessage webview
      ~listener:(onDidReceiveMessage_listener ~document)
      ()
  in
  WebView.set_html webview (get_html_for_WebView_from_file ());
  let _ =
    Workspace.onDidChangeTextDocument
      ~listener:(onDidChangeTextDocument_listener ~webview ~document)
      ()
  in
  transform_to_ast ~document ~webview;
  (* let _ = _on_hover document webview in *)
  CustomTextEditorProvider.ResolvedEditor.t_of_js (Ojs.variable "null")

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

let _onDidSaveTextDocument_listener_pp document =
  let _ =
    let origin_uri = doc_string_uri ~document in
    match StringMap.find !origin_to_pp_doc_map origin_uri with
    | Some pp_uri ->
      let open Promise.Syntax in
      let* original_document =
        Workspace.openTextDocument (`Uri (Uri.parse pp_uri ()))
      in
      manage_changed_origin ~document:original_document
    | None -> Promise.resolve 1
  in
  ()

let onDidChangeActiveTextEditor_listener e =
  if not (TextEditor.t_to_js e |> Ojs.is_null) then
    let document = TextEditor.document e in
    match
      StringMap.find !pp_doc_to_changed_origin_map (doc_string_uri ~document)
    with
    | Some true ->
      let _ = manage_changed_origin ~document in
      ()
    | Some false -> print_endline ("its false" ^ doc_string_uri ~document)
    | _ -> ()
  else
    ()

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
    Workspace.onDidSaveTextDocument
      ~listener:onDidSaveTextDocument_listener_pp ()
  in
  Vscode.ExtensionContext.subscribe extension ~disposable;
  let disposable =
    Vscode.Window.registerCustomEditorProvider ~viewType:"ast-editor"
      ~provider:editorProvider
  in
  Vscode.ExtensionContext.subscribe extension ~disposable

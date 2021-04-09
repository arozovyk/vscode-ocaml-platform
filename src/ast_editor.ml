open Import
open Ppx_utils
module WebViewStore = Map.Make (String)

(*TODO: decreaste the number on disposed webviews, and replace the function with
  proper webview store mechanism*)
let number_of_visible_webviews = ref 0

(*doesnt work because of the bug where the Webview doesn't increment the
  totalColumnCount*)
let _rightmost_column () =
  let open ViewColumn in
  let to_int col = t_to_js col |> Ojs.int_of_js in
  let of_int i = Ojs.int_to_js i |> t_of_js in
  let visibleTextEditors = Window.visibleTextEditors () in
  (* let _ = List.iter visibleTextEditors ~f:(fun x -> let i = to_int ( match
     TextEditor.viewColumn x with | Some column -> column | None -> One ) in
     print_endline (Int.to_string i)) in *)
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

let webview_map = ref WebViewStore.empty

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

  let open_pp_doc ~document =
    let str = get_preprocessed_structure (get_pp_path ~document) in
    let pp_ast = Format.asprintf "%a" Pprintast.structure str in
    let open Promise.Syntax in
    let* doc =
      let textDocumentOptions =
        let open Workspace in
        { language = "ocaml"; content = ocamlformat pp_ast }
      in
      Workspace.openTextDocument (`Interactive (Some textDocumentOptions))
    in
    (* let* _ = TextDocument.save doc in *)
    let+ _ =
      Window.showTextDocument ~document:(`TextDocument doc)
        ~column:ViewColumn.Beside ()
    in
    0

  let manage_choice choice ~document : int Promise.t =
    let make_p c = Promise.resolve c in
    let buildCmd () =
      Sys.command
        ("cd " ^ project_root_path ~document ^ ";eval $(opam env); dune build")
    in
    let rec build_project () =
      let open Promise.Syntax in
      if buildCmd () = 0 then
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
          match WebViewStore.find !webview_map (document_id document) with
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
    WebViewStore.set !webview_map ~key:(document_id document) ~data:webview;

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

let register extension =
  let editorProvider =
    `CustomEditorProvider
      (CustomTextEditorProvider.create ~resolveCustomTextEditor)
  in
  let disposable =
    Vscode.Window.registerCustomEditorProvider ~viewType:"ast-editor"
      ~provider:editorProvider
  in
  Vscode.ExtensionContext.subscribe extension ~disposable

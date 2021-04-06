open Import
open Ppx_utils

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

let get_html_for_WebView_from_file =
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

let onDidChangeTextDocument_listener event ~(document : TextDocument.t)
    ~(webview : WebView.t) =
  let changed_document = TextDocumentChangeEvent.document event in
  if document_eq document changed_document then
    transform_to_ast ~document ~webview
  else
    ()

let onDidReceiveMessage_listener msg ~(document : TextDocument.t) =
  let cbegin = Int.of_string (Ojs.string_of_js (Ojs.get msg "begin")) in
  let cend = Int.of_string (Ojs.string_of_js (Ojs.get msg "end")) in

  let visibleTextEditors =
    List.filter (Vscode.Window.visibleTextEditors ()) ~f:(fun editor ->
        document_eq (TextEditor.document editor) document)
  in
  let apply_selection editor =
    TextEditor.set_selection editor
      (Selection.makePositions
         ~anchor:(Vscode.TextDocument.positionAt document ~offset:cbegin)
         ~active:(Vscode.TextDocument.positionAt document ~offset:cend))
  in
  List.iter ~f:apply_selection visibleTextEditors

let on_hover custom_doc webview =
  let hover =
    Hover.make
      ~contents:
        (`MarkdownString (MarkdownString.make ~value:"hover is working" ()))
  in
  let provideHover ~(document : TextDocument.t) ~(position : Position.t)
      ~(token : CancellationToken.t) =
    let _ = token in
    let offset = TextDocument.offsetAt document ~position in
    if document_eq custom_doc document then
      send_msg "focus" (Ojs.int_to_js offset) ~webview
    else
      ();
    `Value (Some [ hover ])
  in

  let provider = HoverProvider.create ~provideHover in
  Vscode.Languages.registerHoverProvider ~selector:(`String "ocaml") ~provider

(*^ (Path.asset "ast_view.js" |> Path.to_string) ^*)
let resolveCustomTextEditor ~(document : TextDocument.t) ~webviewPanel ~token :
    CustomTextEditorProvider.ResolvedEditor.t =
  let _ = document in
  let _ = token in
  let webview = WebviewPanel.webview webviewPanel in
  let options = WebView.options webview in
  WebviewOptions.set_enableScripts options true;
  WebView.set_options webview options;
  let _ =
    WebView.onDidReceiveMessage webview
      ~listener:(onDidReceiveMessage_listener ~document)
      ()
  in
  WebView.set_html webview get_html_for_WebView_from_file;

  (*let disposable = Commands.registerCommand
    ~command:Extension_consts.Commands.open_to_the_side_ast_preview
    ~callback:(fun ~args:_ -> WebviewPanel.reveal webviewPanel ViewColumn.Two
    ~preserveFocus:false) in Vscode.ExtensionContext.subscribe extension
    ~disposable;*)
  (*let disposable = Commands.registerCommand
    ~command:Extension_consts.Commands.open_to_the_side_ast_preview
    ~callback:(fun ~args:_ -> WebviewPanel.reveal webviewPanel ViewColumn.Two
    ~preserveFocus:false) in Vscode.ExtensionContext.subscribe extension
    ~disposable*)
  let _ =
    Workspace.onDidChangeTextDocument
      ~listener:(onDidChangeTextDocument_listener ~webview ~document)
      ()
  in
  transform_to_ast ~document ~webview;
  let _ = on_hover document webview in

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

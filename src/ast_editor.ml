open Import

let document_eq a b =
  String.equal
    (Uri.toString (TextDocument.uri a) ())
    (Uri.toString (TextDocument.uri b) ())

let get_html_for_WebView_from_file =
  let filename = Node.__dirname () ^ "/../react-app/dist/index.html" in
  In_channel.read_all filename

let transform_to_ast ~(document : TextDocument.t) ~(webview : WebView.t) =
  let value = TextDocument.getText document () |> Dumpast.transform in
  let msg = Ojs.empty_obj () in
  Ojs.set msg "type" (Ojs.string_to_js "setValue");
  Ojs.set msg "value" (Jsonoo.t_to_js value);
  let _ = WebView.postMessage webview msg in
  ()

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

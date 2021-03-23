open Import

let get_html_for_WebView_from_file =
  let filename = Node.__dirname () ^ "/../react-app/dist/index.html" in
  In_channel.read_all filename

let onDidReceiveMessageListener msg ~(document : TextDocument.t) =
  let cbegin = Int.of_string (Ojs.string_of_js (Ojs.get msg "begin")) in
  let cend = Int.of_string (Ojs.string_of_js (Ojs.get msg "end")) in
  let eq a b =
    String.equal
      (Uri.toString (TextDocument.uri a) ())
      (Uri.toString (TextDocument.uri b) ())
  in
  let visibleTextEditors =
    List.filter (Vscode.Window.visibleTextEditors ()) ~f:(fun editor ->
        eq (TextEditor.document editor) document)
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
      ~listener:(onDidReceiveMessageListener ~document)
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
  let listener _event =
    let value = TextDocument.getText document () |> Dumpast.transform in
    let msg = Ojs.empty_obj () in
    Ojs.set msg "type" (Ojs.string_to_js "setValue");
    (*Ojs.set msg "value" (Jsonoo.t_to_js _value);*)
    (*let parseResult = Lift_test.to_js (TextDocument.getText document ()) in*)
    Ojs.set msg "value" (Jsonoo.t_to_js value);
    let _ = WebView.postMessage webview msg in
    ()
  in
  let _ = Workspace.onDidChangeTextDocument ~listener () in
  (* Vscode.ExtensionContext.subscribe extension ~disposable; *)
  CustomTextEditorProvider.ResolvedEditor.t_of_js (Ojs.variable "null")

(*let listener event = let document = TextDocumentChangeEvent.document event in *)

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

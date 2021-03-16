open Import

let get_html_for_WebView webview () =
  let webviewUri =
    WebView.asWebviewUri webview
      ~localResource:(Path.asset "ast_view.js" |> Path.to_string |> Uri.file)
    |> Uri.toString
  in
  " <!DOCTYPE html>\n\
  \  <html lang=\"en\">\n\
   <head>\n\
  \        <title>PPM Preview</title>\n\
  \        <meta charset=\" UTF-8\n\
  \      \">\n\
   <meta http-equiv=\"Content-Security-Policy\" content=\"default-src self; \
   img-src vscode-resource:; script-src vscode-resource: 'self' \
   'unsafe-inline'; style-src vscode-resource: 'self' 'unsafe-inline'; \"/>\n\n\
  \          <style>\n\
  \          #canvas {\n\
  \            background-color: #347890;\n\
  \            position: fixed;\n\
  \            left: 50%;\n\
  \            top: 50%;\n\
  \            transform: translate(-50%, -50%);\n\
  \          }\n\
  \        </style>\n\
  \      </head>\n\
  \    <body>\n\n\
  \   <textarea style=\" background-color:#347890; width: 700px; height: \
   700px;\"></textarea>\n\n\
  \      \n\
  \      <script src=\"" ^ webviewUri ()
  ^ "\" nonce=123 type =\"module\"></script>\n  </body>\n  </html>"

(*^ (Path.asset "ast_view.js" |> Path.to_string) ^*)
let resolveCustomTextEditor ~(document : TextDocument.t) ~webviewPanel ~token :
    CustomTextEditorProvider.ResolvedEditor.t =
  let _ = token in
  let webview = WebviewPanel.webview webviewPanel in
  let options = WebView.options webview in
  WebviewOptions.set_enableScripts options true;
  WebView.set_options webview options;
  (*let _ = WebView.onDidReceiveMessage webview ~listener () in*)
  (*Vscode.ExtensionContext.subscribe extension ~disposable:();*)
  WebView.set_html webview (get_html_for_WebView webview ());
  let listener _event =
    let value = TextDocument.getText document () |> Dumpast.transform in
    let msg = Ojs.empty_obj () in
    Ojs.set msg "type" (Ojs.string_to_js "setValue");
    Ojs.set msg "value" (Ojs.string_to_js value);
    let _ = WebView.postMessage webview msg in
    ()
  in
  let _ = Workspace.onDidChangeTextDocument ~listener () in
  (*Vscode.ExtensionContext.subscribe extension ~disposable; *)
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

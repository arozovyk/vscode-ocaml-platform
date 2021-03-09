open Import

let get_html_for_WebView () =
  " <html lang=\"en\"> <head>    <title>Ast preview </title></head>  \
   <body>         <button>Pow!</button>      </div></div>i'm \
   working!</body></html>"

let resolveCustomTextEditor ~(document : TextDocument.t) ~webviewPanel ~token :
    CustomTextEditorProvider.ResolvedEditor.t =
  let _ = document in
  let _webview = WebviewPanel.webview webviewPanel in
  let _ = WebView.set_html _webview (get_html_for_WebView ()) in
  let _ = token in

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

open Import
open Ppx_utils

let html text =
  "<!DOCTYPE html>\n\
  \  <html>\n\
  \  <body>\n\
  \  \n\
  \  \n\
  \  <textarea  rows=\"50\" cols=\"50\">" ^ text
  ^ "\n\n  </textarea>\n    \n  </body>\n  </html>\n  "

let resolveCustomTextEditor ~(document : TextDocument.t) ~webviewPanel ~token :
    CustomTextEditorProvider.ResolvedEditor.t =
  let _ = document in
  let _ = token in
  let webview = WebviewPanel.webview webviewPanel in
  let options = WebView.options webview in
  WebviewOptions.set_enableScripts options true;
  WebView.set_options webview options;
  let str = get_preprocessed_structure (get_pp_path ~document) in
  let pp_ast = Format.asprintf "%a" Pprintast.structure str in
  print_endline (ocamlformat pp_ast);
  WebView.set_html webview (ocamlformat pp_ast |> html);

  CustomTextEditorProvider.ResolvedEditor.t_of_js (Ojs.variable "null")

let register extension =
  let editorProvider =
    `CustomEditorProvider
      (CustomTextEditorProvider.create ~resolveCustomTextEditor)
  in
  let disposable =
    Vscode.Window.registerCustomEditorProvider ~viewType:"pp-editor"
      ~provider:editorProvider
  in
  Vscode.ExtensionContext.subscribe extension ~disposable

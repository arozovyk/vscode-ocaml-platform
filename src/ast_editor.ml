open Import

let canvasScript () =
  "// @ts-check\n\
  \  (function () {\n\
  \      \n\
   // @ts-ignore\n\n\
  \      const vscode = acquireVsCodeApi();\n\
  \  \n\
  \    const textArea = document.querySelector('textarea');\n\
  \  \n\
  \    const initialState = vscode.getState();\n\
  \    if (initialState) {\n\
  \      textArea.value = initialState.value;\n\
  \    }\n\
  \  \n\
  \    window.addEventListener('message', e => {\n\
   console.log(e.data);\n\n\
  \          switch (e.data.type) {\n\
  \        case 'fakeInput':\n\
  \          {\n\
  \            const value = e.data.value;\n\n\
  \              textArea.value = value;\n\
  \            onInput();\n\
  \            break;\n\
  \          }\n\
  \  \n\
  \        case 'setValue':\n\
  \          {\n\
  \            const value = e.data.value;\n\
  \  console.log(value);\n\n\
  \              textArea.value = value;\n\
  \            vscode.setState({ value });\n\
  \  \n\
  \            vscode.postMessage({\n\
  \              type: 'didChangeContent',\n\
  \              value: value\n\
  \            });\n\
  \            break;\n\
  \          }\n\
  \      }\n\
  \    });\n\
  \  \n\
  \    const onInput = () => {\n\
  \        const value = textArea.value;\n\n\
  \        vscode.setState({ value });\n\
  \      vscode.postMessage({\n\
  \        type: 'edit',\n\
  \        value: value\n\
  \      });\n\
  \      vscode.postMessage({\n\
  \        type: 'didChangeContent',\n\
  \        value: value\n\
  \      });\n\
  \    };\n\
  \  \n\
  \    textArea.addEventListener('input', onInput);\n\
  \  }());"

let get_html_for_WebView () =
  " <!DOCTYPE html>\n\
  \  <html lang=\"en\">\n\
   <head>\n\
  \        <title>PPM Preview</title>\n\
  \        <meta charset=\" UTF-8\n\
  \      \">\n\
  \        <style>\n\
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
  \    <script>" ^ canvasScript () ^ "</script>\n  </body>\n  </html>"

(*^ (Path.asset "ast_view.js" |> Path.to_string) ^*)
let resolveCustomTextEditor ~(document : TextDocument.t) ~webviewPanel ~token :
    CustomTextEditorProvider.ResolvedEditor.t =
  let _ = token in
  (*Fixme(?): Probably wrong module def.*)
  let workSpaceEdit = WorkspaceEdit.make () in
  let listener _obj =
    let range =
      Range.makeCoordinates ~startLine:0 ~startCharacter:0
        ~endLine:(TextDocument.lineCount document)
        ~endCharacter:0
    in
    WorkspaceEdit.replace workSpaceEdit
      ~uri:(TextDocument.uri document)
      ~range ~newText:"test"
  in
  let webview = WebviewPanel.webview webviewPanel in
  let options = WebView.options webview in
  WebviewOptions.set_enableScripts options true;
  WebView.set_options webview options;
  let _ = WebView.onDidReceiveMessage webview ~listener () in
  (*Vscode.ExtensionContext.subscribe extension ~disposable:();*)
  WebView.set_html webview (get_html_for_WebView ());
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

//var JSONFormatter = require('../node_modules/json-formatter-js/dist/json-formatter.js').default;
// @ts-check
(function () {
    // @ts-ignore
    const vscode = acquireVsCodeApi();

    const textArea = document.querySelector('textarea');

    const initialState = vscode.getState();
    if (initialState) {
        textArea.value = initialState.value;
    }
    textArea.addEventListener("mouseover", function (event) {
        vscode.postMessage({
            type: 'mouseover',
            value: 'h'
        });
    });


    window.addEventListener('message', e => {
        var _ast = e.data.value
        var para = document.createElement("p");
        var node = document.createTextNode("This is new.");
        para.appendChild(node);
        var element = document.getElementById("ast");
        const myJSON = { ans: 42 };
        //const formatter = new JSONFormatter(myJSON);
       // element.appendChild(formatter);

        switch (e.data.type) {
            case 'setValue':
                {
                    const value = e.data.value;
                    textArea.value = value;
                    vscode.setState({ value });
                    vscode.postMessage({
                        type: 'didChangeContent',
                        value: value
                    });
                    break;
                }
        }
    });

    /* const onInput = () => {
         const value = textArea.value;
         vscode.setState({ value });
         vscode.postMessage({
             type: 'edit',
             value: value
         });
         vscode.postMessage({
             type: 'didChangeContent',
             value: value
         });
     };
 
     textArea.addEventListener('input', onInput);*/
}());

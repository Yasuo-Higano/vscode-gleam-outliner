// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require('vscode');
const languageclient = require("vscode-languageclient");

let starter = `${__dirname}/gleam_outliner.sh`;
let win_starter = `${__dirname}\\gleam_outliner.bat`;

if( __dirname.indexOf('\\') >= 0 ) {
    starter = win_starter;
}

console.log(`starter: ${starter}`);
// this method is called when your extension is activated
// your extension is activated the very first time the command is executed

/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {
    vscode.window.showInformationMessage("gleam-outliner is now active! ");

    try {
        const serverOptions = {
            command: starter,
            args: [
            ]
        };

        const clientOptions = {
            documentSelector: [
                {
                    scheme: "file",
                    language: "gleam",
                }
            ],
        };
        var client = new languageclient.LanguageClient("gleam-outliner", serverOptions, clientOptions);
        context.subscriptions.push(client.start());
    } catch (e) {
        vscode.window.showErrorMessage("gleam-outliner couldn't be started.");
    }
}

// this method is called when your extension is deactivated
function deactivate() {}

module.exports = {
	activate,
	deactivate
}

// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require('vscode');
// const languageclient = require("vscode-languageclient");
// //const LanguageClient, {TransportKind } = require('vscode-languageclient/node');
// const net = require('net');
const net = require('net');
const { LanguageClient, TransportKind } = require('vscode-languageclient/node');

const { exec } = require('child_process')
//const cp = require('child_process');
//const util = require('util');

const config = vscode.workspace.getConfiguration('gleam_outliner');
const port = parseInt( config.get('port') );
const debug_log = config.get('debug_log');
//const reload = " --reload"
const reload = "";

let starter = `${__dirname}/gleam_outliner_stdio.sh`;
let win_starter = `${__dirname}\\gleam_outliner.bat`;

if( __dirname.indexOf('\\') >= 0 ) {
    starter = win_starter;
}

console.log(`starter: ${starter}`);
// this method is called when your extension is activated
// your extension is activated the very first time the command is executed

/*
const exec = util.promisify(cp.exec);
//
async function runLanguageServer_stdio(cmdline,port) {
    try {
        const { stdout, stderr } = await exec(cmdline+" --port "+port);
        //const { stdout, stderr } = await exec(cmdline+" --reload --port "+port);
        const output = stdout.trim();
        return output;
    } catch (error) {
        console.error(`# gleam: Error executing script: ${error}`);
    }
}
*/

function runLanguageServer_sock(cmdline,port,next) {
    function nx() {
            try {
                next();
                vscode.window.showInformationMessage("gleam-outliner is now active! ");
            }
            catch(ex) {
                console.log("## gleam-outliner couldn't be started.", ex);
                vscode.window.showErrorMessage("gleam-outliner couldn't be started.");
            }
    }
    let logcmd = "";
    if( debug_log) {
        logcmd = " --debug-log ~/gleam-outliner.log";
    }
    exec(cmdline+logcmd+reload+" --port "+port,(err, stdout, stderr) =>{
        nx();
        //setTimeout(nx,30000);
    } );
}

/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {
    //activate_using_socket(context);
    activate_using_stdio(context);
}

function activate_using_stdio(context) {
    vscode.window.showInformationMessage("activating gleam-outliner(stdio)...");

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
        var client = new LanguageClient("gleam-outliner", serverOptions, clientOptions);
        context.subscriptions.push(client.start());
    } catch (e) {
        console.log("## gleam-outliner couldn't be started.", e);
        vscode.window.showErrorMessage("gleam-outliner couldn't be started.");
    }
}

function activate_using_socket(context) {

    vscode.window.showInformationMessage("activating gleam-outliner(socket)...");
    function init() {
        let server = "localhost";

        const serverOptions = () => {
            const socket = net.connect({ port: port });
            const result = {
                writer: socket,
                reader: socket,
            };
            return Promise.resolve(result);
        };
        

        const clientOptions = {
            documentSelector: [
                {
                    scheme: "file",
                    language: "gleam",
                }
            ],
            
        };
        var client = new LanguageClient(
            "gleam-outliner",
            "Gleam Outliner",
             serverOptions,
             clientOptions,
             TransportKind.socket
            ).start();
        context.subscriptions.push(client);
        console.log("## gleam-outliner started.");
    }
    runLanguageServer_sock(starter,port,init);

}

// this method is called when your extension is deactivated
function deactivate() {}

module.exports = {
	activate,
	deactivate
}

const {app, BrowserWindow, net, dialog} = require("electron");
const {exec} = require("child_process");
const fs = require("fs");

let win, server;

function createWindow() {
    win = new BrowserWindow({width: 1024, height: 768});
    win.loadURL("http://localhost:3000");
    win.on("closed", () => { win = null });
}

app.on("ready", () => {
    if (!fs.existsSync("./imdb/actors.list")) {
	dialog.showMessageBox({ message: "IMDb files not found. " +
				"Did you copy them into the app bundle?" });
	exec("open ./imdb/", function() {
	    app.quit();
	});
    }
    
    server = exec("./moviz-server", function(error, stdout, stderr) {
	dialog.showMessageBox({ message: stdout + stderr });
	app.quit();
    });
    createWindow();
});

app.on("window-all-closed", () => {
    if (process.platform !== "darwin")
	app.quit();
});

app.on("activate", () => {
    if (win === null)
	createWindow();
});

app.on("quit", (e) => {
    server.kill("SIGKILL");
});

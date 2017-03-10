const {app, BrowserWindow, net, dialog, Menu, ipcMain} = require("electron");
const {exec} = require("child_process");
const fs = require("fs");

let win, server, quitting, builtMenu;

const menuTemplate = [
    { label: "Edit", submenu: [
	{ role: "undo" }, { role: "redo" }, { type: "separator" }, { role: "cut" },
	{ role: "copy" }, { role: "paste" }, { role: "selectall" } ] },
    { role: "window", submenu: [
	{ role: "minimize" }, { role: "close" } ] },
    { role: "help", submenu: [] } ];

if (process.platform === "darwin") {
    menuTemplate.unshift({
	label: app.getName(), submenu: [
	    { label: "About moviz", click() { execJS(true, "$('#info').click();"); } },
	    { label: "Load graph", accelerator: "CmdOrCtrl+O",
	      click() { execJS(false, "$('#load').click();"); } },
	    { label: "Save graph", accelerator: "CmdOrCtrl+S",
	      click() { execJS(false, "$('#save').click();"); } },
	    { type: "separator" }, { role: "services", submenu: [] },
	    { type: "separator" }, { role: "hide" }, { role: "hideothers" },
	    { role: "unhide" }, { type: "separator" }, { role: "quit" } ] });
    menuTemplate[2].submenu = [
	{ label: "Close", accelerator: "CmdOrCtrl+W", role: "close" },
	{ label: "Minimize", accelerator: "CmdOrCtrl+M", role: "minimize" },
	{ label: "Zoom", role: "zoom" },
	{ type: "separator" },
	{ label: "Bring All to Front", role: "front" } ] };

function execJS(create) {
    var args = Array.prototype.slice.call(arguments, 1);
    var win = getWindow(create);
    if (win)
	win.webContents.executeJavaScript.apply(win.webContents, args);
}

function getWindow(create) {
    if (win)
	return win;

    if (create === false)
	return null;

    win = new BrowserWindow({
	width: 1024, height: 768, minWidth: 400, minHeight: 200,
	titleBarStyle: "hidden-inset", show: false
    });
    global.win = win;
    global.setProgressBar = win.setProgressBar.bind(win);
    win.loadURL("http://localhost:3000");
    win.on("closed", () => { win = null });
    win.once("ready-to-show", () => {
	win.show();
    });

    ipcMain.on("app-ready", () => {
	execJS(false, "App.debug", false, function(debug) {
	    if (debug && !builtMenu) {
		menuTemplate.push({ label: "Debug", submenu: [
		    { role: "reload" }, { role: "forcereload" }, { role: "toggledevtools" } ] });
		Menu.setApplicationMenu(Menu.buildFromTemplate(menuTemplate));
		builtMenu = true;
	    }
	});
    });
    
    return win;
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
	if (!quitting) {
	    dialog.showMessageBox({ message: stdout + stderr });
	    app.quit();
	}
    });

    Menu.setApplicationMenu(Menu.buildFromTemplate(menuTemplate));
    getWindow();
});

app.on("window-all-closed", () => {
    if (process.platform !== "darwin")
	app.quit();
});

app.on("activate", () => {
    getWindow();
});

app.on("quit", (e) => {
    quitting = true;
    server.kill("SIGKILL");
});

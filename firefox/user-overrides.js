// Personal overrides appended after Betterfox's user.js.
//
// user.js is re-applied on every Firefox start, so any pref set by hand in
// about:config is lost. Put custom prefs here instead. This file has to come
// after Betterfox so these values take precedence.

// --- One tab per window -----------------------------------------------------
// Links that would open a tab open a new window instead.
user_pref("browser.link.open_newwindow", 2);
// Apply that to every case, including JS popups.
user_pref("browser.link.open_newwindow.restriction", 0);
// Links from other apps also get their own window.
user_pref("browser.link.open_newwindow.override.external", 2);
// Middle-click opens a window, not a tab.
user_pref("browser.tabs.opentabfor.middleclick", false);

// --- Zoom links -------------------------------------------------------------
// Hand zoommtg:// and zoomus:// links to the Zoom app.
user_pref("network.protocol-handler.expose.zoommtg", false);
user_pref("network.protocol-handler.external.zoommtg", true);
user_pref("network.protocol-handler.expose.zoomus", false);
user_pref("network.protocol-handler.external.zoomus", true);

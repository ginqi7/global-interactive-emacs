function run(argv) {
    // console.log(argv[0])
    const appName = argv[0];
    const tabId = argv[1];
    const chrome = Application (appName);
    const windows  = chrome.windows;
    for (i in windows) {
        const tabs = windows[i].tabs;
        for (j in tabs) {
            if (tabId == tabs[j].id()) {
                tabs[j].close();
                return;
            }
        }
    }
}

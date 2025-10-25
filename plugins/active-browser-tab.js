function run(argv) {
    // console.log(argv[0])
    const appName = argv[0];
    const tabId = argv[1];
    const chrome = Application(appName);
    const windows  = chrome.windows;
    for (i in windows) {
        const tabs = windows[i].tabs;
        for (j in tabs) {
            if (tabId == tabs[j].id()) {
                const index = +j + 1; // j is a string
                windows[i].activeTabIndex = (index);
                return;
            }
        }
    }
}

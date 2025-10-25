function run(argv) {
    // console.log(argv[0])
    const appName = argv[0]
    const chrome = Application (appName);
    const windows  = chrome.windows;
    var output = [];
    for (i in windows) {
        const tabs = windows[i].tabs;
        for (j in tabs) {
            output.push( {
                "id" : tabs[j].id(),
                "title" : tabs[j].title(),
                "url" :tabs[j].url(),
            });
        }
    }
    console.log(JSON.stringify(output))
}

osascript -e 'tell application "System Events" to get name of (processes where background only is false)' | sed  's/, /\n/g'

tell application "Google Chrome"
    set output to ""
    repeat with t in (tabs of (first window whose index is 1))
           set output to output & title of t & "\n"
    end repeat
    output
end tell


-- tell application "Google Chrome"
--      activate         
--     -- set output to ""
--     -- repeat with t in (tabs of (first window whose index is 1))
--     --        set output to output & title of t & "\n"
--     -- end repeat
--     -- output
-- end tell

tell application "System Events"
    tell process "Google Chrome"

    repeat with t in (tabs of (first window whose index is 1))
           set output to output & title of t & "\n"
    end repeat


    end tell
end tell


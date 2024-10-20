(*
	AppleScript for enabling F# keys on Mac OS X while playing Starcraft II. Settings will revert to
	default after the game has closed.
	
	Works with 10.7.3
	
	Author: Paul Lambert (notpml @ github) 
*)


on run
	
	-- toggle F# keys from Apple functionality to standard button usage
	set check to true
	toggle_fn_keys(check)
	
	set app_name to "SC2"
	ignoring application responses
		do shell script "open -a \"/Applications/Starcraft II/Starcraft II.app/Contents/MacOS/Starcraft II\""
	end ignoring
	
	delay 30
	
	repeat while app_is_running(app_name)
		delay 5
	end repeat
	
	-- return F# keys to their original settings (brightness, volume, etc.)
	set check to false
	toggle_fn_keys(check)
	
	tell me to quit
	
end run

-- toggle F# keys on or off
on toggle_fn_keys(check_that_box)
	tell application "System Preferences"
		set current pane to pane id "com.apple.preference.keyboard"
		tell application "System Events"
			tell process "System Preferences"
				
				-- get current value of our checkbox 
				set is_checked to value of checkbox "Use all F1, F2, etc. keys as standard function keys" of tab group 1 of window "Keyboard"
				
				-- if it's currently unchecked and we want it checked, click it!
				if check_that_box is true and is_checked is not 1 then
					click checkbox "Use all F1, F2, etc. keys as standard function keys" of tab group 1 of window "Keyboard"
				end if
				
				-- if it's currently checked and we want it unchecked, click it!
				if check_that_box is false and is_checked is not 0 then
					click checkbox "Use all F1, F2, etc. keys as standard function keys" of tab group 1 of window "Keyboard"
				end if
			end tell
		end tell
	end tell
end toggle_fn_keys

on app_is_running(appName)
	tell application "System Events" to set appNameIsRunning to exists (processes where name is appName)
	return appNameIsRunning
end app_is_running
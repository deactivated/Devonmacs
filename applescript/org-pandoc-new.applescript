tell application id "com.devon-technologies.thinkpro2"
	set active_selection to selection
	set selection_name to name of item 1 of active_selection
	set selection_path to path of item 1 of active_selection
	set appname to (path to frontmost application as Unicode text)
	do shell script "emacsclient -n -e '(org-pandoc-new nil \"'" & quoted form of appname & "'\")'"
end tell

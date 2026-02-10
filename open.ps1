# Source - https://superuser.com/a/1896742
# Posted by Vomit IT - Chunky Mess Style, modified by community. See post 'Timeline' for change history
# Retrieved 2026-01-03, License - CC BY-SA 4.0

$folders = "C:\Users\plewe\Documents\GitHub",
	   "C:\Users\plewe\Documents\GitHub\ConsTcl1",
	   "C:\Users\plewe\Documents\GitHub\ConsTcl1\tests",
	   "C:\Users\plewe\Documents\GitHub\ConsTcl1\tests\tmp";

$wshell = New-Object -ComObject WScript.Shell;

Start-Process "explorer.exe" "Documents";
Start-Sleep -Milliseconds 500; 

$wshell.AppActivate("Documents");
Start-Sleep -Seconds 1;

$wshell.SendKeys("%");     # Alt
$wshell.SendKeys("{TAB}"); # TAB

$folders | ForEach-Object {
    Start-Sleep -Milliseconds 500; 
    $wshell.SendKeys("^{t}");  # Ctrl+T
    Start-Sleep -Milliseconds 500; 
    $wshell.SendKeys("%d");    # Alt+D
    $wshell.SendKeys("$_{ENTER}");
    };

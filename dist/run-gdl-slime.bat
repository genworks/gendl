@echo off

cd %~dp0\..\

if not "%1"=="" goto :start
:noimpl
echo USAGE: run-gdl-slime.bat c:\path\to\lisp
echo   This command will read a line of lisp forms to start the swank
echo   server from stdin and start a lisp that runs these forms.
exit 1

:start
set /p swank_command="Enter the commands necessary to start the swank server on one line (slime will do this for you)"

echo Starting swank server in lisp %1...

%1 +B +m -e '%swank_command%'
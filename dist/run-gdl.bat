cd %~dp0\program\

if exist "..\..\gpl\gnu-emacs-23.3\bin\runemacs.exe" (set emacspath=..\..\gpl\gnu-emacs-23.3\bin\runemacs.exe) else (set emacspath=..\gpl\gnu-emacs-23.3\bin\runemacs.exe)
 
%windir%\system32\cmd.exe "/C start %emacspath% --no-splash --no-init-file -l ..\emacs\gdl.el"

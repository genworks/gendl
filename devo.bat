cd %~dp0

%windir%\system32\cmd.exe "/C start ..\..\common\gpl\gnu-emacs-23.3\bin\runemacs.exe --no-splash --no-init-file -l devo.el -f slime"

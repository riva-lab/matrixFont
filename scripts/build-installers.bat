@chcp 1251
echo off

set INNOSETUPCOMPILER="C:\Program Files (x86)\Inno Setup 6\iscc.exe"
set OPTIONS=

%INNOSETUPCOMPILER% %OPTIONS% "installer\installer-win32.iss"
%INNOSETUPCOMPILER% %OPTIONS% "installer\installer-win64.iss"

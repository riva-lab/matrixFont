@chcp 1251
echo off

set MDPATH=..\help
set TEMPLATE=%MDPATH%\matrixFont-help-template.html
set MARKDOWN=%MDPATH%\matrixFont-help.md
set OUTHTML=%MDPATH%\matrixFont-help.html

copy /b %TEMPLATE% + %MARKDOWN% %OUTHTML% /y
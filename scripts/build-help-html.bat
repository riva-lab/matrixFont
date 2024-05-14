@chcp 1251
echo off

set MDPATH=..\help
set MDPROJNAME=matrixFont
set TEMPLATE=%MDPATH%\%MDPROJNAME%-help-template.html
set MARKDOWN=%MDPROJNAME%-help*

cmd /c gen-html-from-md.bat
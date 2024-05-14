@chcp 1251
echo off

set MDPATH=..\
set TEMPLATE=%MDPATH%\readme-template.html
set MARKDOWN=readme*

cmd /c gen-html-from-md.bat
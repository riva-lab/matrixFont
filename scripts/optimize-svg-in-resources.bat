set TEMPDIR=tempb4Ta9hW5bzE3

cd ..\resources
md %TEMPDIR%

for %%i in (*.svg) do (scour %%i %TEMPDIR%\%%i --enable-viewboxing --enable-id-stripping --enable-comment-stripping --shorten-ids --indent=none)
xcopy %TEMPDIR%"\*.svg" "*.svg" /y

rd %TEMPDIR% /s /q

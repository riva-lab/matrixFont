@echo off
rem https://stackoverflow.com/questions/1706892/how-do-i-retrieve-the-version-of-a-file-from-a-batch-file-on-windows-vista

setlocal
set "item=%~1"
set "item=%item:\=\\%"


for /f "usebackq delims=" %%a in (`"WMIC DATAFILE WHERE name='%item%' get Version /format:Textvaluelist"`) do (
    for /f "delims=" %%# in ("%%a") do set "%%#"
)

if "%~2" neq "" (
    endlocal & (
        echo %version%
        set %~2=%version%
    )
) else (
    echo %version%
)


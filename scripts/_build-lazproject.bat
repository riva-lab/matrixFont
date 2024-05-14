@chcp 1251
echo off

set LAZBUILD=c:\lazarus\lazbuild.exe
set PROJFILE=_matrixFont.lpi

cd "..\source"
rd /s /q "lib"

setlocal enabledelayedexpansion
for %%a in (32,64) do (
    set PROJARC=win%%a
    set BUILDMODE=!PROJARC!-Release

    echo.
    echo Building Lazarus project:
    echo  - name: %PROJFILE%
    echo  - mode: !BUILDMODE!
    echo ------------------------

    "%LAZBUILD%" --no-write-project --build-mode=!BUILDMODE! %PROJFILE%
)
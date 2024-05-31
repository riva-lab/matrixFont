@chcp 1251
echo off



echo.
echo Set paths for utilities
echo.

set SEVENZIP_EXECUTABLE=7z
set PO_UTILITY=scripts\tools\poFileUtility.exe



echo.
echo Project settings
echo.

set PROJNAME=matrixFont
set BUILD=Release
set LANGDIR=bin\lang
set COMMONFILES=%LANGDIR%\*.ini %LANGDIR%\%PROJNAME%.pot %LANGDIR%\%PROJNAME%.??.po %LANGDIR%\%PROJNAME%.?????.po readme.* license.* versions.* help\*


echo.
echo Creating html help file
echo.
call "build-help-html.bat"

echo.
echo Creating html readme file
echo.
call "build-readme-html.bat"

echo.
echo Creating output directory for current app version

FOR /F "delims=" %%i IN ('get-version.bat "%cd%\..\bin\%PROJNAME%-win64-Release.exe"') DO set EXEVER=%%i
set DEST=install\v%EXEVER%
echo  - directory: %DEST%



cd ..

echo.
echo Removing win32 localization files
echo.
del /f /q %LANGDIR%\*win32-*.po?

echo.
echo Copying win64 localization files to files shared by all binaries
echo.
copy %LANGDIR%\%PROJNAME%-win64-%BUILD%.*.po  %LANGDIR%\%PROJNAME%.*.po

echo.
echo  Copying win64 localization template into common template
echo.
copy %LANGDIR%\%PROJNAME%-win64-%BUILD%.pot   %LANGDIR%\%PROJNAME%.pot

echo.
echo Transfer lines in localization file for original language and save to .ru.po
echo.
%PO_UTILITY% %LANGDIR%\%PROJNAME%.pot %LANGDIR%\%PROJNAME%.ru.po transfer


setlocal enabledelayedexpansion
for %%a in (32,64) do (
    set PROJARC=win%%a
    set BINARY=bin\*!PROJARC!-%BUILD%.exe
    set FILES=!BINARY! %COMMONFILES%
    set FILENAME=%DEST%\%PROJNAME%-v%EXEVER%-!PROJARC!-Portable.zip

    echo.
    echo Creating archive:
    echo  - ZIP: {!FILENAME!}
    echo  - Files: {!FILES!}
    echo ------------------------

    del /f /q "!FILENAME!"
    
    "%SEVENZIP_EXECUTABLE%" a -tzip -mx5 !FILENAME! !FILES!
)


@chcp 1251
echo off

set DD=%DATE:~0,2%
set MM=%DATE:~3,2%
set YY=%DATE:~8,2%
set YYYY=%DATE:~6,4%
set HH=%TIME:~0,2%
set MN=%TIME:~3,2%

set DATE_STAMP=%YYYY%-%MM%-%DD%_%HH%-%MN%

set UPX_EXECUTABLE=upx
set SEVENZIP_EXECUTABLE=7z
set PO_UTILITY=scripts\tools\poFileUtility.EXE

set DEST=install\%DATE_STAMP%
set PROJNAME=matrixFont
set BUILD=Release

set LANGDIR=bin\lang
set LANGINI=%LANGDIR%\*.ini
set LANG=%LANGDIR%\%PROJNAME%.??.po
set LANGTMP=%LANGDIR%\%PROJNAME%.pot
set HELP=help\%PROJNAME%-Help.*
set FILES_ADDITION=readme.md license.md versions.md help/*


cd ..

echo.
echo Удаление файлов перевода x32
echo.
del /f /q %LANGDIR%\*x32-*.po?

echo.
echo Копирование x64 файлов перевода в общие для всех бинарников
echo.
copy %LANGDIR%\%PROJNAME%-x64-%BUILD%.*.po  %LANGDIR%\%PROJNAME%.*.po

echo.
echo Копирование x64 файлов шаблона перевода в общий
echo.
copy %LANGDIR%\%PROJNAME%-x64-%BUILD%.pot   %LANGDIR%\%PROJNAME%.pot

echo.
echo Перенос строк в файле перевода для языка оригинала в .ru.po
echo.
%PO_UTILITY% %LANGDIR%\%PROJNAME%.pot %LANGDIR%\%PROJNAME%.ru.po transfer



echo.
echo Создаем архив для x64
echo.

set PROJARC=x64

set BINARY=bin\*%PROJARC%-%BUILD%.exe
set LIBS=

set FILENAME="%DEST%\%PROJNAME%-%PROJARC%-Portable.zip"
set FILES="%BINARY%" "%LANG%" "%LANGTMP%" "%LANGINI%" "%HELP%"

del /f /q %FILENAME%

"%UPX_EXECUTABLE%"       --best "%BINARY%"
"%SEVENZIP_EXECUTABLE%"  a -tzip -mx5 %FILENAME% %FILES% %FILES_ADDITION%



echo.
echo Создаем архив для x32
echo.

set PROJARC=x32

set BINARY=bin\*%PROJARC%-%BUILD%.exe
set LIBS=

set FILENAME="%DEST%\%PROJNAME%-%PROJARC%-Portable.zip"
set FILES="%BINARY%" "%LANG%" "%LANGTMP%" "%LANGINI%" "%HELP%"

del /f /q %FILENAME%

"%UPX_EXECUTABLE%"       --best "%BINARY%"
"%SEVENZIP_EXECUTABLE%"  a -tzip -mx5 %FILENAME% %FILES% %FILES_ADDITION%

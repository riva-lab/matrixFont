@chcp 1251
echo off



echo.
echo Текущие дата и время
echo.

set DD=%DATE:~0,2%
set MM=%DATE:~3,2%
set YY=%DATE:~8,2%
set YYYY=%DATE:~6,4%
set HH=%TIME:~0,2%
set MN=%TIME:~3,2%

set DATE_STAMP=%YYYY%-%MM%-%DD%_%HH%-%MN%



echo.
echo Задаем пути исполняемых файлов утилит
echo.

set UPX_EXECUTABLE=upx
set SEVENZIP_EXECUTABLE=7z
set PO_UTILITY=scripts\tools\poFileUtility.EXE



echo.
echo Настройки проекта
echo.

set PROJNAME=matrixFont

set BUILD=Release
set DEST=install\%DATE_STAMP%

set LANGDIR=bin\lang
set LANGINI=%LANGDIR%\*.ini
set LANG=%LANGDIR%\%PROJNAME%.??.po
set LANGTMP=%LANGDIR%\%PROJNAME%.pot
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
set FILES="%BINARY%" "%LANG%" "%LANGTMP%" "%LANGINI%" %FILES_ADDITION%

del /f /q %FILENAME%

"%UPX_EXECUTABLE%"       --lzma         "%BINARY%"
"%SEVENZIP_EXECUTABLE%"  a -tzip -mx5   %FILENAME%  %FILES%



echo.
echo Создаем архив для x32
echo.

set PROJARC=x32

set BINARY=bin\*%PROJARC%-%BUILD%.exe
set LIBS=

set FILENAME="%DEST%\%PROJNAME%-%PROJARC%-Portable.zip"
set FILES="%BINARY%" "%LANG%" "%LANGTMP%" "%LANGINI%" %FILES_ADDITION%

del /f /q %FILENAME%

"%UPX_EXECUTABLE%"       --lzma         "%BINARY%"
"%SEVENZIP_EXECUTABLE%"  a -tzip -mx5   %FILENAME%  %FILES%

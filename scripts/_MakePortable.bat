@chcp 1251

set DD=%DATE:~0,2%
set MM=%DATE:~3,2%
set YY=%DATE:~8,2%
set YYYY=%DATE:~6,4%
set HH=%TIME:~0,2%
set MN=%TIME:~3,2%

set DATE_STAMP=%YYYY%-%MM%-%DD%_%HH%-%MN%

set UPX_EXECUTABLE=upx
set SEVENZIP_EXECUTABLE=7z

set DEST=install\%DATE_STAMP%
set PROJNAME=matrixFont
set BUILD=Release


cd ..

set PROJARC=x64

set BINARY=bin\*%PROJARC%-%BUILD%.exe
set LANG=bin\lang\*%PROJARC%-%BUILD%.??.po
set LANGINI=bin\lang\*.ini
set HELP=help\%PROJNAME%-Help.*
set LIBS=

set FILENAME="%DEST%\%PROJNAME%-%PROJARC%-Portable.zip"
set FILES="%BINARY%" "%LANG%" "%LANGINI%" "%HELP%"
set FILES_ADDITION=readme.md license.md versions.md help/*

del /f /q %FILENAME%

"%UPX_EXECUTABLE%"       --best "%BINARY%"
"%SEVENZIP_EXECUTABLE%"  a -tzip -mx5 %FILENAME% %FILES% %FILES_ADDITION%



set PROJARC=x32

set BINARY=bin\*%PROJARC%-%BUILD%.exe
set LANG=bin\lang\*%PROJARC%-%BUILD%.??.po
set LANGINI=bin\lang\*.ini
set HELP=help\%PROJNAME%-Help.*
set LIBS=

set FILENAME="%DEST%\%PROJNAME%-%PROJARC%-Portable.zip"
set FILES="%BINARY%" "%LANG%" "%LANGINI%" "%HELP%"

del /f /q %FILENAME%
copy /y bin\lang\*x64-%BUILD%.??.po bin\lang\*x32-%BUILD%.??.po

"%UPX_EXECUTABLE%"       --best "%BINARY%"
"%SEVENZIP_EXECUTABLE%"  a -tzip -mx5 %FILENAME% %FILES% %FILES_ADDITION%

del /f /q bin\lang\*x32-%BUILD%.??.po

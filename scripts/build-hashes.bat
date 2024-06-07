@chcp 1251
echo off

set PROJNAME=matrixFont

rem Get current executable version to variable EXEVER
FOR /F "delims=" %%i IN ('get-version.bat "%cd%\..\bin\%PROJNAME%-win64-Release.exe"') DO set EXEVER=%%i

rem Go to executables location
cd ..\install\v%EXEVER%

echo Project '%PROJNAME%' MD5 and SHA hashes for v%EXEVER% executables> hashes.txt
echo:>> hashes.txt

echo Executables location:>> hashes.txt
cd   >> hashes.txt
echo:>> hashes.txt
echo:>> hashes.txt
echo:>> hashes.txt

rem Calc hashes for all project executables
forfiles /m %PROJNAME%* /c "cmd /c ..\..\scripts\get-file-hashes.bat @file"

rem Open hashes file
hashes.txt
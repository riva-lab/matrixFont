@chcp 1251

set DD=%DATE:~0,2%
set MM=%DATE:~3,2%
set YY=%DATE:~8,2%
set YYYY=%DATE:~6,4%
set HH=%TIME:~0,2%
set MN=%TIME:~3,2%

set DATE_STAMP=%YYYY%-%MM%-%DD%_%HH%-%MN%
set PROJNAME=Icons_Pack
set DEST=.
set ARCHIVE=Pack

set A16=active 16
set A32=active 32
set D16=disabled 16
set D32=disabled 32
set H16=hot 16
set H32=hot 32
set SRC=Source

"%homedrive%\Program Files\7-Zip\7z.exe" a -tzip -mx0 "%DEST%\%ARCHIVE%\%PROJNAME%_%DATE_STAMP%.zip" "%DEST%\%A16%" "%DEST%\%A32%" "%DEST%\%D16%" "%DEST%\%D32%" "%DEST%\%H16%" "%DEST%\%H32%" "%DEST%\%SRC%" "%DEST%\*.png" "%DEST%\*.spl7"
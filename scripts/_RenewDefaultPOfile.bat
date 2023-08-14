@chcp 1251

set UTILITY=..\poFileUtility\bin\poFileUtility.EXE
set FILE=BIN\LANG\matrixFont-x64-Release.ru.po

cd ..
%UTILITY% %FILE% %FILE% transfer


echo off

setlocal
set "item=%~1"
set "item=%item:\=\\%"

echo %item% >> hashes.txt
echo ------------------------------>> hashes.txt

echo MD5>> hashes.txt
CertUtil -hashfile %item% MD5 >> hashes.txt
echo:>> hashes.txt

echo SHA>> hashes.txt
CertUtil -hashfile %item% SHA1 >> hashes.txt
echo:>> hashes.txt

echo:>> hashes.txt
echo:>> hashes.txt

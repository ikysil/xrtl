call clean.bat
svn export . export --force
cd export
7z -bd -tzip a ..\XRTL-src-yyyy-mm-dd-v.v.zip -r * -x!*.dcu -x!docs\api\*
cd ..
call gendoc.bat
7z -bd -tzip a XRTL-doc-yyyy-mm-dd-v.v.zip -r docs\api\*.html
rmdir/s/q export

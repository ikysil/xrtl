call clean.bat
rmdir/s/q export
svn export . export --force
cd export
7z -bd -tzip a ..\XRTL-yyyy-mm-dd-v.v.zip -r * -x!*.dcu
cd ..
rmdir/s/q export

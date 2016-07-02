cd ..
git submodule update --init
cd dastworx

#iz sources
cd ../etc/iz/import/
iz=$(find `pwd` -type f -name \*.d)
cd ../../../dastworx

#dparse sources
cd ../etc/libdparse/src/
dparse=$(find `pwd` -type f -name \*.d)
cd ../../../dastworx

#dast sources
cd src/
dast=$(find `pwd` -type f -name \*.d)
cd ../

echo building...

#build
dmd ${dast[@]} ${dparse[@]} ${iz[@]} \
-O -release -inline -boundscheck=off \
-Isrc -I../etc/iz/import -I../etc/libdparse/src \
-of../bin/dastworx

#cleanup
rm ../bin/dastworx.o

echo ...done
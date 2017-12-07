ver=`cat version.txt`
fld=coedit-x86
cd nux32
mkdir $fld/
cp * $fld/
zip -9 \
../output/coedit.${ver:1:100}.linux32.zip \
$fld/dcd.license.txt $fld/coedit.license.txt \
$fld/coedit $fld/dastworx \
$fld/coedit.ico $fld/coedit.png \
$fld/dcd-server $fld/dcd-client $fld/dscanner
rm -rf coedit-x86

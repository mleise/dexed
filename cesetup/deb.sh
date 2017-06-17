ver=`cat version.txt`
maj=${ver:0:1}
min1=${ver//_}
min=${min1:1}
dte=$(LC_TIME='en_EN.UTF-8' date -u +"%a %b %d %Y")

if [ $CPU = "x86_64" ]; then
    arch="amd64"
else
    arch="i386"
fi

name=coedit-$maj-$min.$arch

basdir=$HOME/$name/
cfgdir=$basdir/DEBIAN
bindir=$basdir/usr/bin
pixdir=$basdir/usr/share/pixmaps
shcdir=$basdir/usr/share/applications
arch=""

mkdir -p $basdir
mkdir -p $cfgdir
mkdir -p $bindir
mkdir -p $pixdir
mkdir -p $shcdir

cp nux64/coedit $bindir
cp nux64/dastworx $bindir
cp nux64/coedit.png $pixdir

echo "[Desktop Entry]
Categories=Application;IDE;Development;
Exec=coedit %f
GenericName=coedit
Icon=coedit
Keywords=editor;Dlang;IDE;dmd;
Name=coedit
StartupNotify=true
Terminal=false
Type=Application" > $shcdir/coedit.desktop
 
cd $cfgdir 
echo "Package: coedit
Version: $maj$min
Section: base
Priority: optional
Architecture: $arch
Depends: bash
Maintainer: Basile Burg <b2.temp@gmx.com>
Description: IDE for the D programming language" > control

cd $HOME
dpkg-deb --build $name

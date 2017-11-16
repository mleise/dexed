ver=`cat version.txt`
maj=${ver:0:1}
min1=${ver//_}
min=${min1:1}
dte=$(LC_TIME='en_EN.UTF-8' date -u +"%a %b %d %Y")
arch=`uname -m`
specname=coedit-$arch.spec

buildroot=$HOME/rpmbuild/BUILDROOT/coedit-$maj-$min.$arch
bindir=$buildroot/usr/bin
pixdir=$buildroot/usr/share/pixmaps
shcdir=$buildroot/usr/share/applications

mkdir -p $buildroot
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

cd $HOME/rpmbuild/SPECS
echo "Name: coedit
Version: $maj
Release: $min
Summary: IDE for the D programming language
License: Boost
URL: www.github.com/BBasile/Coedit

%description
Coedit is an IDE for the DMD D compiler.

%files
/usr/bin/dastworx
/usr/bin/coedit
/usr/share/applications/coedit.desktop
/usr/share/pixmaps/coedit.png

%changelog
* $dte Basile Burg b2.temp@gmx.com
- see https://github.com/BBasile/Coedit/releases/tag/$ver
">$specname

rpmbuild -ba $specname

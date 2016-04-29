buildroot=$HOME/rpmbuild/BUILDROOT/coedit
bindir=$buildroot/usr/bin
pixdir=$buildroot/usr/share/pixmaps
shcdir=$buildroot/usr/share/applications

mkdir -p $buildroot
mkdir -p $bindir
mkdir -p $pixdir
mkdir -p $shcdir

cp nux64/coedit $bindir
cp nux64/cesyms $bindir
cp nux64/cetodo $bindir
cp nux64/coedit.png $pixdir

echo "[Desktop Entry]
Categories=Application;IDE;Development;
Exec=coedit %f
GenericName=coedit
Icon=/usr/share/pixmaps/coedit.png
Keywords=editor;Dlang;IDE;dmd;
Name=coedit
StartupNotify=true
Terminal=false
Type=Application" > $shcdir/coedit.desktop

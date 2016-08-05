::iz sources
set iz=
for /r "../etc/iz/import/" %%F in (*.d) do call set iz=%%iz%% "%%F"

::dparse sources
set dparse=
for /r "../etc/libdparse/src/" %%F in (*.d) do call set dparse=%%dparse%% "%%F"

::dast sources
set dast=
for /r "src/" %%F in (*.d) do call set dast=%%dast%% "%%F"

echo building...

::build
dmd %dast% %dparse% %iz% ^
-O -release -inline -boundscheck=off ^
-Isrc -I"..\etc\iz\import" -I"..\etc\libdparse\src" ^
-of"..\bin\dastworx"

::cleanup
del ..\bin\dastworx.o

echo ...done
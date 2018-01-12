---
title: Build Coedit
---

{% raw %}
<script src="//cdnjs.cloudflare.com/ajax/libs/anchor-js/4.0.0/anchor.min.js"></script>
{% endraw %}

### Build

#### Coedit

Coedit is mostly programmed in Object Pascal, using the the [Lazarus development platform](http://www.lazarus-ide.org/).

* [Download](http://lazarus.freepascal.org/index.php?page=downloads) and setup the latest Lazarus version (>= 1.6.4) and  FPC + FPC sources (>= 3.0.2) for your platform.
    * Windows: the three packages are bundled in an installer. Even on Windows 64 bit, the 32 version must be setup.
    * Linux: the three packages must be downloaded and setup individually. It's recommended to download the packages from _SourceForge_ and not from the official repository of the distribution because they don't always propose the latest version.
* `cd <user dir where to clone>`
* `git clone https://github.com/BBasile/Coedit.git`
* `git submodule update --init --recursive`, to clone the dependencies used by the background tool.

The Lazarus LCL and the FreePascal FCL may require patches that fix bugs or regressions present in the latest Lazarus release and for which Coedit cannot include workarounds.
Any `.patch` file located in the `patches/` folder should be applied. On linux you'll have to set the write permissions to `/usr/lib64/fpc` and `/usr/lib64/lazarus`.

You're now ready to build Coedit. This can be done in the IDE or using the _lazbuild_ utility.

* With _Lazarus_: 
    * start Lazarus,
    * in the **project** menu, click *open...* and select the file **Coedit.lpi**, which is located in the sub-folder **lazproj**.
    * in the menu **Execute** click **Create**.

* With _lazbuild_: 
    * open a console.
    * `cd` to the repository location, sub folder **lazproj**.
    * type `lazbuild -B coedit.lpi` and <kbd>ENTER</kbd>. note that the path to _lazbuild_ may have to be specified.

After what Coedit should be build. The executable is output to the _bin_ folder.

#### Dastworx

The background tool used by the IDE is a D program.

* [Download](https://dlang.org/download.html#dmd) and setup latest DMD version.
* In the repository, browse to the `dastworx` folder.
    * Windows: double click `build.bat`
    * Linux: `sh ./build.sh`

You can also build it in CE using the project file _dastworx.ce_.

#### Third party tools:

Additionally you'll have to build [the completion daemon **DCD**](https://github.com/BBasile/DCD#setup) and the [D linter **Dscanner**](https://github.com/Hackerpilot/Dscanner#building-and-installing).
See the products documentation for more information.

{% raw %}
<script>
anchors.add();
</script>
{% endraw %}

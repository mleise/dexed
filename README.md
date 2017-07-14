<p align="center">
  <img src="https://github.com/BBasile/Coedit/raw/master/logo/coedit.png?raw=true" width="15%" height="15%" alt="CE"/>
</p>

Coedit is an IDE for the [D programming language](http://dlang.org), its compilers, tools and libraries.

[![](http://bbasile.github.io/Coedit/img/coedit_kde4_thumb.png)](http://bbasile.github.io/Coedit/img/coedit_kde4.png)

**Overview**
---
- available for _Windows_ or _Linux_.
- supports all the D compilers (DMD-GDC-LDC).
- supports the DUB projects (JSON + SDL) and also Coedit own project format.
- support the DUB scripts (aka _single file packages_) and Coedit own script format (aka _runnable modules_).
- full [D Completion Daemon](https://github.com/dlang-community/DCD) integration (completion, ddoc display, call tips, jump to declaration, rename identifier).
- Dynamic [D-Scanner](https://github.com/dlang-community/D-Scanner) linting with results displayed in the editor gutter.
- single click to compile and to _unittest_ a module.
- advanced editor with D2 syntax highlighter, folds, regions, identifier markup, macros, sync-edit, etc.
- edition helpers: comment blocks, local identifier renaming, brace auto-closing, ddoc templates, etc.
- Explorable list of symbols of the current module.
- static libraries manager that supports auto-registration from local DUB projects, from online DUB packages or from Coedit custom project format.
- todo list based on the _todo comments_ located in a project or in the current source.
- user-defined tools powered by a string interpolation system.
- mini file browser, dfmt interface, search & replace, [discover more in the manual](http://bbasile.github.io/Coedit/).

**Project information**
---

- latest release: version 3, update 3, Fri 14 Jul 2017.
- licensed under the terms of the Boost software license.
- Development can be supported by [donating here](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=AQDJVC39PJF7J).

**Download version 3 update 3**
---
Download the zipped binaries or the zip that contains the setup program for you platform:

- :package: [setup program for Linux 32 bit](https://github.com/BBasile/Coedit/releases/download/3_update_3/coedit.3update3.linux32.setup.zip)
- :package: [binaries for Linux 32 bit](https://github.com/BBasile/Coedit/releases/download/3_update_3/coedit.3update3.linux32.zip)
- :package: [setup program for Linux 64 bit](https://github.com/BBasile/Coedit/releases/download/3_update_3/coedit.3update3.linux64.setup.zip)
- :package: [binaries for Linux 64 bit](https://github.com/BBasile/Coedit/releases/download/3_update_3/coedit.3update3.linux64.zip)
- :package: [setup program for Windows 32/64 bit](https://github.com/BBasile/Coedit/releases/download/3_update_3/coedit.3update3.win32.setup.zip)
- :package: [binaries for Windows 32/64 bit](https://github.com/BBasile/Coedit/releases/download/3_update_3/coedit.3update3.win32.zip)
- :package: [rpm for Linux 64 bit](https://github.com/BBasile/Coedit/releases/download/3_update_3/coedit-3-update3.x86_64.rpm)
- :package: [deb for Linux 64 bit](https://github.com/BBasile/Coedit/releases/download/3_update_3/coedit-3-update3.amd64.deb)

The _zip_ allow to move freely the files.
The _setup.zip_ install to predefined locations so that the software can be run without additional intervention.
The _deb_ and the _rpm_ are for those who prefer the official setup system for their linux systems.

[**See this page**](http://bbasile.github.io/Coedit/setup.html) for more information about the setup.

**Build**
---

Follow the procedure described [**here**](http://bbasile.github.io/Coedit/build.html).

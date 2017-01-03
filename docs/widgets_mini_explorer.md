---
title: Widgets - mini explorer
---

### Mini explorer

#### Description

The mini explorer provides basic file browsing within the IDE.

![](img/mini_explorer.png)

- ![](https://raw.githubusercontent.com/BBasile/Coedit/master/icons/folder/folder_add.png): Adds the selected folder to the favorites.
- ![](https://raw.githubusercontent.com/BBasile/Coedit/master/icons/folder/folder_delete.png): Removes the selected favorite folder.
- ![](https://raw.githubusercontent.com/BBasile/Coedit/master/icons/other/flash.png): Open the selected folder or execute the selected file using the shell.
-  ![](https://raw.githubusercontent.com/BBasile/Coedit/master/icons/other/pencil.png): If the selected file is a CE or a DUB project then opens it as a project otherwise opens it in a new code editor.
- ***input field***: filter the files whose name contains the text typed.

The file list supports drag and drop.

#### Options

A few options are available in the [option editor](widgets_options_editor).

![](img/options_mini_explorer.png)

- **contextExpands**: If checked then the tree auto expands to the folder that contains the source or the project file that's been selected.
- **doubleClick**: Defines what happens when a file is double clicked.
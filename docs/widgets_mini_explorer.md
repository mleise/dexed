---
title: Widgets - mini explorer
---

{% include xstyle.css %}

### Mini explorer

#### Description

The mini explorer provides basic file browsing functionality within the IDE.

![](img/mini_explorer.png)

- <img src="{%include icurl%}folder/folder_add.png" class="tlbric"/>: Adds the selected folder to the favorites.
- <img src="{%include icurl%}folder/folder_delete.png" class="tlbric"/>: Removes the selected favorite folder.
- <img src="{%include icurl%}other/flash.png" class="tlbric"/>: Open the selected folder or execute the selected file using the shell.
- <img src="{%include icurl%}other/pencil.png" class="tlbric"/>: If the selected file is a CE or a DUB project then opens it as a project otherwise opens it in a new code editor.
- ***input field***: filter the files whose name contains the text typed.

The file list supports drag and drop.

#### Options

A few options are available in the [option editor](widgets_options_editor).

![](img/options_mini_explorer.png)

- **contextExpands**: If checked then the tree auto expands to the folder that contains the source or the project file that's been selected.
- **doubleClick**: Defines what happens when a file is double clicked.
---
title: Widgets - options editor
---

### Options editor

The _Options editor_ is a special, non-dockable, widget that allows the other widgets to expose their options.
The list at the left displays the categories. A category often matches to a single widget but not only (for example the shortcuts).

![](img/options_application.png)

The options are applied in real time but are reversible until the green checker icon is clicked.

- ![](https://raw.githubusercontent.com/BBasile/Coedit/master/icons/other/accept.png): validates the modifications made to the current category, after what they can't be canceled anymore.
- ![](https://raw.githubusercontent.com/BBasile/Coedit/master/icons/other/cancel.png): cancels and restores the previous state of the current category.

The options are persistent and saved in a distinct folder:

- Linux:
**`/home/<your account>/.config/Coedit/`**.
- Windows:
**`?:\Users\<your account>\AppData\Roaming\Coedit\`**.

Each widget and software component save their own files with a self-explanatory name so it's easy to find and modify the file that matches a particular setting.
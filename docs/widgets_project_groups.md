---
title: Widgets - project groups
---

{% include xstyle.css %}

### Project groups

#### Description

Project groups ease the way to work with several projects, it's easier to activate a one, recompile it and to go back to another one. 
A group can include any project whose the format is handled by Coedit.

Another interesting feature is that the groups can be build by a single click, in parallel, sequentially or using wait points which are defined for each item in the group. When working with static libraries, this system allows faster builds.

Even if a group would not be used to build, for example with DUB since it manages the dependencies, it's still interesting to create the group, just to ease the selection of a project and to open more easily one of its source from the [project inspector](widgets_project_inspector).

The actions operated on the group are available from the **Projects group** menu. The widget is only used to modify the items.

![](img/widgets_projects_groups.png)

The groups don't affect the workflow and the feature can be totally ignored to preserve the way CE worked in version 1 and 2. 
A project is not part of the group until it's explicitly added. The project that has this independent status is called the _Free Standing Project_ (FSP).
The FSP is actually a project, as it got handled in the previous versions.

#### Toolbar

- <img src="{%include icurl%}file/document_add.png" class="tlbric"/>: Adds a new project from an open dialog.
- <img src="{%include icurl%}file/document_delete.png" class="tlbric"/>: Removes the select project from the group.
- <img src="{%include icurl%}arrow/arrow_up.png" class="tlbric"/>: Moves the selected project to the top. This modifies the order of construction.
- <img src="{%include icurl%}arrow/arrow_down.png" class="tlbric"/>: Moves the selected project to the bottom. This modifies the order of construction.
- <img src="{%include icurl%}arrow/arrow_divide.png" class="tlbric"/>: When the last icon indicates this state and if the group is build using the wait points than this project is build in a new parallel process.
- <img src="{%include icurl%}arrow/arrow_join.png" class="tlbric"/>: When the last icon indicates this state and if the group is build using the wait points than this project is not build until the previous project are build. A wait point is often used for the last item since the binaries produced by the other projects have to be linked in.

The field at the bottom indicates the status of the FSP.

- <img src="{%include icurl%}other/pencil.png" class="tlbric"/>: Activates the FSP.
- <img src="{%include icurl%}file/document_add.png" class="tlbric"/>: Adds the FSP to the group.
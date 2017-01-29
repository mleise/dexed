---
title: Symbolic strings
---

### Symbolic strings

The symbolic strings represent variables defined by the software. They are used by several widgets:

- the [runnable modules shebang line](features_runnables).
- the [native project confirguration widget](widgets_native_project_configuration): many fields accept symbols.
- the [custom tools](widgets_custom_tools): parameters, working directory.
- the [process input](widgets_process_input): the input field can include a symbol.
- the [GDB commander](widgets_gdb_commander) custom commands.

Possible symbols, by context, include:

Application:

- **`<CAF>`**: also _`<CoeditApplicationFile>`_. Expanded to Coedit filename.
- **`<CAP>`**: also _`<CoeditApplicationPath>`_. Expanded to Coedit path.

Environment:

- **`<ENV_HOME>`**: Expanded to the user home directory. This is the equivalent of `HOME` (on a Posix system) or `USERHOME` (on a Windows system).
- **`<ENV_TEMP>`**: Expanded to the user temporary directory.
- **`<ENV_USER>`**: Expanded to the user name.

Current file:

- **`<CFF>`**: also _`<CurrentFileFile>`_. Expanded to the current file filename.
- **`<CFP>`**: also _`<CurrentFilePath>`_. Expanded to the current file path.
- **`<CFR>`**: also _`<CurrentFileRunnable>`_. Expanded to the runnable produced for the current file. The [OutputFolder](features_runnables) option is not handled.
- **`<CI>`**: also _`<CurrentIdentifier>`_. Expanded to the identifier located at the caret position.
- **`<CL>`**: also _`<CurrentLine>`_. Expanded to the current line of code.
- **`<CS>`**: also _`<CurrentSelection>`_. Expanded to the current selection.

Current project:

- **`<CPF>`**: also _`<CurrentProjectFile>`_. Expanded to the project filename.
- **`<CPFS>`**: also _`<CurrentProjectFiles>`_. Expanded to a list that contains each D source of the project. Each item is separated by a line ending.
- **`<CPN>`**: also _`<CurrentProjectName>`_. Expanded to the project name (it's filename minus its path and extension)
- **`<CPO>`**: also _`<CurrentProjectOutput>`_. Expanded to the project output filename (a static library filename, a program name, etc.)
- **`<CPOP>`**: also _`<CurrentProjectOutputPath>`_. Expanded to the project output parent directory.
- **`<CPP>`**: also _`<CurrentProjectPath>`_. Expanded to the project path.
- **`<CPR>`**: also _`<CurrentProjectRoot>`_. Expanded to the field _RootFolder_ of a CE project (n/a if the current project is for DUB).
- **`<CPCD>`**: also _`<CurrentProjectCommonFilesDirectory>`_. Expanded to the sources common directory.

The expanded form of a symbol is never empty. When a symbol expands to nothing it's set to a pair of back quotes, e.g **\``** for a **`<CPP>`** when no project is opened.

The slices located before and after the symbols are passed in their original form, e.g:

`http://www.google.com/search?q="dlang.org"<CI>&btnI=Im+Feeling+Lucky`

is expanded to: 

`http://www.google.com/search?q="dlang.org"indexOf&btnI=Im+Feeling+Lucky` if **indexOf** is the current identifier. 

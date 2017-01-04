---
title: Runnable modules
---

### Runnable modules

#### Description

Coedit is able to compile and execute the module that's currently edited even if it's not part of a project.
Such a module is called a **runnable** module.

Runnable modules don't have to be explicitly saved because Coedit will handle the task automatically, using an unique temporary file name.
For example it's possible to execute the _Compile and run file_ action directly after the _New runnable module_ action .
By default the binary is produced in the folder where is located the D source but this can be changed in the options applied to this feature.

A runnable is useful to quickly test an idea, learn how to use new libraries, or even to use modules as scripts, always without messing with the parameters that a project would require.

#### Shebang line

By default runnables don't need any setting however the shebang line can be used when specific compiler options are required.
Two typical scenarios:

- the runnable will be debugged so DWARF information must be generated with `-g`.
- the runnable must be linked with a C static library so the linker flag `-L-lthelib` must be specified.

Coedit doesn't handle the program specified after the She-Bang, which means that any of the following script lines are valid:

- `#!runnable-flags: --whatever`
- `#!usr/bin/dmd --whatever`
- `#!options --whatever`

In the three cases, every non white character after the She-Bang is ignored.
Options are detected after the first non white character of the script line.

#### Runnable I/O handling

In general the program output is redirected to the [messages](widgets_messages).
This is true unless the _Compile file and run outside_ or the _Run compiled file outside_ actions are used.

When the program is not run outside, the [process input widget](widgets_process_input) is used to pass input to the runnable.

#### Other

To be runnable, a module must verify:

- a `void main()` is present.
- the modules to import must be known, either by the [library manager](widgets_library_manager) or by the compiler configuration file.
- _import expressions_ ( `import(file);` ) are allowed if _file_ stands in the same folder as the module being edited.

The _Compile file and run outside_ action can be used to execute in an external console.
This must be used if the runnable outputs thousands of lines, to display properly UTF8 characters or if it has a GUI.

The version identifier **runnable_module** is automatically defined when a runnable is compiled.
It can be used to adjust the source according to the execution context, for example:

```d
version(runnable_module)
{
    stdout.writeln("to output stream");
}
else
{
    myFile.writeln("to a file");
}
```

The executable produced is deleted after each run unless the file has been saved explicitly out of the initial temporary folder.
Note that the action _Run file unittest_ is based on the same internal function excepted that the `-main` and `-unittest` switches are automatically added to the switch list (menu **File**, action **Set runnable switches**).

#### Options

![](img/options_runnables.png)

- __alwaysToFolder__: When checked the folder specified in __outputFolder__ is handled, even if the runnable module is not part of the current project.
- __compiler__: Select the [compiler](options_compilers_paths) used to produce the runnable binary. When GDC or LDC is selected their bridges based on DMD command line interface are used (GDMD and LDMD).
- __detectLibraries__: When checked the static libraries used by the runnable are detected from the [library manager](widgets_library_manager) by performing import analysis. When unchecked, all the library manager entries are passed and the compiler does the selection.
- __detectMain__: When checked the `main()` function is detected automatically and the `-main` switch is set accordingly. When not checked `-main` is never passed. This options is useful with the **Run file unittests** action because it allows to test a module that's also a valid program.
- __outputFolder__: Defines a folder where the runnable binary is output. If the runnable is part of the project this folder is used otherwise __alwaysToFolder__ must also be checked. When the folder is a relative path, it is solved using the runnable module parent directory as root.
- __staticSwitches__: Defines a list of switches that are always passed to the compiler when a runnable is produced or when a module is tested.

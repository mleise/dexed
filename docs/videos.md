---
title: Usage in video
---

{% raw %}
<script src="//cdnjs.cloudflare.com/ajax/libs/anchor-js/4.0.0/anchor.min.js"></script>
{% endraw %}

Picked from a dedecated [playlist](https://www.youtube.com/playlist?list=PLzk8A0LUvEOV-OMdz09jfOahwnKoA2na_), and with description.

### Debug a runnable

This video shows how a [runnable module](features_runnables) can be debugged in the [GDB commander](widgets_gdb_commander) widget.
The runnable calls a function that returns an `int`.
A breakpoint is put before returning and the result is patched in the CPU inspector by changing RAX value.

{% raw %}
<iframe width="560" height="315" src="https://www.youtube.com/embed/HbEPkA1EfaU" frameborder="0" allowfullscreen></iframe>
{% endraw %}

### GIT gui as a tool

This video shows how to setup a [custom tool](widgets_custom_tools) that invokes `git gui` for the current project.
The trick is to set the tool's current working directory with a [symbolic string](features_symbolic_strings) that's expanded to the current project location.

{% raw %}
<iframe width="560" height="315" src="https://www.youtube.com/embed/sRmc-CDcx2I" frameborder="0" allowfullscreen></iframe>
{% endraw %}

### Compiler paths

This video shows how it's easy to [select a specific D compiler](options_compilers_paths) on the fly: official dmd, upstream dmd, ldc and gdc.
Here the choice is applied to the compiler used to make a runnable but the same setting also exists for the projects.

{% raw %}
<iframe width="560" height="315" src="https://www.youtube.com/embed/RuisTY6m_3E" frameborder="0" allowfullscreen></iframe>
{% endraw %}

### Library manager and runnables

This video shows how a DUB library can be downloaded, installed and used directly in a [runnable](features_runnables) module.
The most important operations are made in the [library manager](widgets_library_manager).
At the end and to make obvious the automatic aspect of a library selection, the runnable is compiled while the new libman entry is deactivated, which results in a compiler error.

{% raw %}
<iframe width="560" height="315" src="https://www.youtube.com/embed/FapAXM5yDLI" frameborder="0" allowfullscreen></iframe>
{% endraw %}

### DDOC templates

This video shows how a DDOC template can be inserted for a new function, with prefilled `Returns` and `Params` sections.

{% raw %}
<iframe width="560" height="315" src="https://www.youtube.com/embed/VEVqSItCKfo" frameborder="0" allowfullscreen></iframe>
{% endraw %}



{% raw %}
<script>
anchors.add();
</script>
{% endraw %}

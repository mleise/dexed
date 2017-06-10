---
title: Options - DUB build
---

{% raw %}
<script src="//cdnjs.cloudflare.com/ajax/libs/anchor-js/4.0.0/anchor.min.js"></script>
{% endraw %}

#### DUB build

This category exposes the DUB options that are passed to the build tool each time it's used.

![](img/options_dub_build.png)

- **combined**: If checked, tries to build the whole project in a single compiler run.
- **compiler**: Selects [which compiler](options_compilers_paths) is used by DUB.
- **dependenciesCheck**: Defines how DUB checks the project dependencies, typically used to avoid too much network operations.
- **forceRebuild**: Forces a full recompilation, even if DUB determines that this would not be necessary.
- **linkMode**: Specifies the way the compiler and linker are invoked.
- **other**: Displays a list that can be filled with more switches. One item per line.
- **parallel**: If checked, tries to build using several CPU.

See also [the official DUB command line reference](http://code.dlang.org/docs/commandline) for more detailed descriptions.

{% raw %}
<script>
anchors.add();
</script>
{% endraw %}

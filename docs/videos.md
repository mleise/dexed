---
title: Usage in video
---

{% raw %}
<script src="//cdnjs.cloudflare.com/ajax/libs/anchor-js/4.0.0/anchor.min.js"></script>
{% endraw %}

### Debug a runnable

This video shows how a [runnable module](features_runnables) can be debugged in the [GDB commander](widgets_gdb_commander) widget.
The runnable calls a function that returns an `int`.
A breakpoint is put before returning and the result is patched in the CPU inspector by changing RAX value.

{% raw %}
<div class="tumblr-post" data-href="https://embed.tumblr.com/embed/post/RfMjxAacl04tQydtQ8GmYA/161537904188" data-did="da39a3ee5e6b4b0d3255bfef95601890afd80709" data-language="en_US"><a href="https://abstractop.tumblr.com/post/161537904188">https://abstractop.tumblr.com/post/161537904188</a></div>  <script async src="https://assets.tumblr.com/post.js"></script>
{% endraw %}

### GIT gui as a tool

This video shows how to setup a [custom tool](widgets_custom_tools) that invokes `git gui` for the current project.
The trick is to set the tool's current working directory with a [symbolic string](features_symbolic_strings) that's expanded to the current project location.

{% raw %}
<div class="tumblr-post" data-href="https://embed.tumblr.com/embed/post/RfMjxAacl04tQydtQ8GmYA/161538065841" data-did="da39a3ee5e6b4b0d3255bfef95601890afd80709" data-language="en_US"><a href="https://abstractop.tumblr.com/post/161538065841">https://abstractop.tumblr.com/post/161538065841</a></div>  <script async src="https://assets.tumblr.com/post.js"></script>
{% endraw %}

### Compiler paths

This video shows how it's easy to [select a specific D compiler](options_compilers_paths) on the fly: official dmd, upstream dmd, ldc and gdc.
Here the choice is applied to the compiler used to make a runnable but the same setting also exists for the projects.

{% raw %}
<div class="tumblr-post" data-href="https://embed.tumblr.com/embed/post/RfMjxAacl04tQydtQ8GmYA/161552390630" data-did="da39a3ee5e6b4b0d3255bfef95601890afd80709"><a href="https://abstractop.tumblr.com/post/161552390630">https://abstractop.tumblr.com/post/161552390630</a></div>  <script async src="https://assets.tumblr.com/post.js"></script>
{% endraw %}

### Library manager and runnables

This video shows how a DUB library can be downloaded, installed and used directly in a [runnable](features_runnables) module.
The most important operations are made in the [library manager](widgets_library_manager).
At the end and to make obvious the automatic aspect of a library selection, the runnable is compiled while the new libman entry is deactivated, which results in a compiler error.

{% raw %}
<div class="tumblr-post" data-href="https://embed.tumblr.com/embed/post/RfMjxAacl04tQydtQ8GmYA/161655243088" data-did="af776982db9ee6d2165b243bceccac121d668407"><a href="https://abstractop.tumblr.com/post/161655243088/coedit-library-manager-and-runnables">https://abstractop.tumblr.com/post/161655243088/coedit-library-manager-and-runnables</a></div>  <script async src="https://assets.tumblr.com/post.js"></script>
{% endraw %}


{% raw %}
<script>
anchors.add();
</script>
{% endraw %}

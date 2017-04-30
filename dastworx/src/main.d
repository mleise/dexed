module dastworx;

import
    core.memory;
import
    std.array, std.getopt, std.stdio, std.path, std.algorithm;
import
    iz.memory;
import
    dparse.lexer, dparse.parser, dparse.ast, dparse.rollback_allocator;
import
    common, todos, symlist, imports, mainfun, halstead, ddoc_template;


private __gshared int caretLine;
private __gshared bool option1;
private __gshared static Appender!(ubyte[]) source;
private __gshared static Appender!(AstErrors) errors;
private __gshared string[] files;

// -o : deep visit the symbols
alias deepSymList = option1;
// -o : outputs /++ +/ ddoc instead of /** */
alias plusComment = option1;

static this()
{
    GC.disable;
    source.reserve(1024^^2);
    errors.reserve(32);
}

void main(string[] args)
{
    version(devel)
    {
        mixin(logCall);
        File f = File(__FILE__, "r");
        foreach(ref buffer; f.byChunk(4096))
            source.put(buffer);
        f.close;
    }
    else
    {
        // get the source to process.
        // even when files are passed, the std input has to be closed by the IDE
        foreach(ref buffer; stdin.byChunk(4096))
            source.put(buffer);
        if (!source.data.length && args.length > 1)
        {
            import std.file: exists;
            files = args[1].splitter(pathSeparator).filter!exists.array;
            version(devel) writeln(files);
        }
    }

    // options for the work
    getopt(args, std.getopt.config.passThrough,
        "o", &option1,
        "l", &caretLine
    );

    // launch directly a work
    getopt(args, std.getopt.config.passThrough,
        "i", &handleImportsOption,
        "m", &handleMainfunOption,
        "s", &handleSymListOption,
        "t", &handleTodosOption,
        "H", &handleHalsteadOption,
        "K", &handleDdocTemplateOption,
    );
}

/// Handles the "-s" option: create the symbol list in the output
void handleSymListOption()
{
    mixin(logCall);

    RollbackAllocator alloc;
    StringCache cache = StringCache(StringCache.defaultBucketCount);
    LexerConfig config = LexerConfig("", StringBehavior.source);

    source.data
        .getTokensForParser(config, &cache)
        .parseModule("", &alloc, &handleErrors)
        .listSymbols(errors.data, deepSymList);
}

/// Handles the "-t" option: create the list of todo comments in the output
void handleTodosOption()
{
    mixin(logCall);
    if (files.length)
        getTodos(files);
}

/// Handles the "-i" option: create the import list in the output
void handleImportsOption()
{
    mixin(logCall);
    if (files.length)
    {
        listFilesImports(files);
    }
    else
    {
        RollbackAllocator alloc;
        StringCache cache = StringCache(StringCache.defaultBucketCount);
        LexerConfig config = LexerConfig("", StringBehavior.source);

        source.data
            .getTokensForParser(config, &cache)
            .parseModule("", &alloc, &ignoreErrors)
            .listImports();
    }
}

/// Handles the "-m" option: writes if a main() is present in the module
void handleMainfunOption()
{
    mixin(logCall);

    RollbackAllocator alloc;
    StringCache cache = StringCache(StringCache.defaultBucketCount);
    LexerConfig config = LexerConfig("", StringBehavior.source);

    source.data
        .getTokensForParser(config, &cache)
        .parseModule("", &alloc, &ignoreErrors)
        .detectMainFun();
}

/// Handles the "-H" option: write the halstead metrics
void handleHalsteadOption()
{
    mixin(logCall);

    RollbackAllocator alloc;
    StringCache cache = StringCache(StringCache.defaultBucketCount);
    LexerConfig config = LexerConfig("", StringBehavior.source);

    source.data
        .getTokensForParser(config, &cache)
        .parseModule("", &alloc, &ignoreErrors)
        .performHalsteadMetrics;
}

/// Handles the "-D" option: write the ddoc template for a given declaration
void handleDdocTemplateOption()
{
    mixin(logCall);

    RollbackAllocator alloc;
    StringCache cache = StringCache(StringCache.defaultBucketCount);
    LexerConfig config = LexerConfig("", StringBehavior.source);

    source.data
        .getTokensForParser(config, &cache)
        .parseModule("", &alloc, &ignoreErrors)
        .getDdocTemplate(caretLine, plusComment);
}

private void handleErrors(string fname, size_t line, size_t col, string message,
    bool err)
{
    errors ~= construct!(AstError)(cast(ErrorType) err, message, line, col);
}

version(devel)
{
    version(none) import std.compiler;
    version(all) import std.uri;
    version(WatchOS) import std.math;
    mixin(q{import std.c.time;});
    // TODO: something
    // NOTE: there was a bug here...
    // FIXME-cmain-aMrFreeze-p8: there's an infinite recursion whith the option -x
}


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
    common, todos, symlist, imports, mainfun, runnableflags;


private __gshared bool storeAstErrors = void, deepSymList;
private __gshared const(Token)[] tokens;
private __gshared Module module_ = void;
private __gshared static Appender!(ubyte[]) source;
private __gshared RollbackAllocator allocator;
private __gshared LexerConfig config;
private __gshared StringCache* cache;
private __gshared static Appender!(AstErrors) errors;
private __gshared string[] files;


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
        config = LexerConfig("", StringBehavior.source, WhitespaceBehavior.skip);
        cache = construct!(StringCache)(StringCache.defaultBucketCount);
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
        if (!source.data.length)
        {
            files = args[1].splitter(pathSeparator).array;
            version(devel) writeln(files);
        }
        // when files are passed, the global cache & config cant be used
        else
        {
            config = LexerConfig("", StringBehavior.source, WhitespaceBehavior.skip);
            cache = construct!(StringCache)(StringCache.defaultBucketCount);
        }
    }

    // options for the worxs
    getopt(args, std.getopt.config.passThrough,
        "d", &deepSymList
    );

    // launch a worx directly
    getopt(args, std.getopt.config.passThrough,
        "i", &handleImportsOption,
        "m", &handleMainfunOption,
        "r", &handleRunnableFlags,
        "s", &handleSymListOption,
        "t", &handleTodosOption,
    );
    stdout.flush;
}

/// Handles the "-s" option: create the symbol list in the output
void handleSymListOption()
{
    mixin(logCall);
    storeAstErrors = true;
    lex!false;
    parseTokens;
    listSymbols(module_, errors.data, deepSymList);
}

/// Handles the "-t" option: create the list of todo comments in the output
void handleTodosOption()
{
    mixin(logCall);
    getTodos(files);
}

/// Handles the "-r" option:
void handleRunnableFlags()
{
    mixin(logCall);
    lex!true;
    getRunnableFlags(tokens);
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
        storeAstErrors = false;
        lex!false;
        parseTokens;
        listImports(module_);
    }
}

/// Handles the "-m" option: writes if a main() is present in the module
void handleMainfunOption()
{
    mixin(logCall);
    storeAstErrors = false;
    lex!false;
    parseTokens;
    detectMainFun(module_);
}

private void handleErrors(string fname, size_t line, size_t col, string message, bool err)
{
    if (storeAstErrors)
        errors ~= construct!(AstError)(cast(ErrorType) err, message, line, col);
}

private void lex(bool keepComments = false)()
{
    static if (keepComments)
        tokens = DLexer(source.data, config, cache).array;
    else
        tokens = getTokensForParser(source.data, config, cache);
}

private void parseTokens()
{
    mixin(logCall);
    if (!module_)
        module_ = parseModule(tokens, "", &allocator, &handleErrors);
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


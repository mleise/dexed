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


private __gshared bool storeAstErrors = void, deepSymList = true;
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
        File f = File(__FILE__, "r");
        foreach(buffer; f.byChunk(4096))
            source.put(buffer);
        f.close;
    }
    else
    {
        foreach(buffer; stdin.byChunk(4096))
            source.put(buffer);
    }

    if (args.length > 2)
    {
        files = args[1].splitter(pathSeparator).array;
        version(devel) writeln(files);
    }

    config = LexerConfig("", StringBehavior.source, WhitespaceBehavior.skip);
    cache = construct!(StringCache)(StringCache.defaultBucketCount);

    getopt(args, std.getopt.config.passThrough,
        "d", &deepSymList
    );

    getopt(args, std.getopt.config.passThrough,
        "i", &handleImportsOption,
        "m", &handleMainfunOption,
        "r", &handleRunnableFlags,
        "s", &handleSymListOption,
        "t", &handleTodosOption,
    );
}

void handleSymListOption()
{
    mixin(logCall);
    bool deep;
    storeAstErrors = true;
    lex!false;
    parseTokens;
    listSymbols(module_, errors.data, deepSymList);
}

void handleTodosOption()
{
    mixin(logCall);
    getTodos(files);
}

void handleRunnableFlags()
{
    mixin(logCall);
    lex!true;
    getRunnableFlags(tokens);
}

void handleImportsOption()
{
    mixin(logCall);
    storeAstErrors = false;
    lex!false;
    parseTokens;
    listImports(module_);
}

void handleMainfunOption()
{
    mixin(logCall);
    storeAstErrors = false;
    lex!false;
    parseTokens;
    detectMainFun(module_);
}

void handleErrors(string fname, size_t line, size_t col, string message, bool err)
{
    if (storeAstErrors)
        errors ~= construct!(AstError)(cast(ErrorType) err, message, line, col);
}

void lex(bool keepComments = false)()
{
    static if (keepComments)
    {
        DLexer dlx = DLexer(source.data, config, cache);
        tokens = dlx.array;
    }
    else
        tokens = getTokensForParser(source.data, config, cache);
}

void parseTokens()
{
    mixin(logCall);
    if (!module_)
        module_ = parseModule(tokens, "", &allocator, &handleErrors);
}

version(devel)
{
    version(none) import std.compiler;
    version(all) import std.uri;
    mixin(q{import std.c.time;});

    // TODO:something
}


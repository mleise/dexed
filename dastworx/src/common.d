module common;

import
    std.array, std.traits, std.meta, std.conv;
import
    dparse.lexer, dparse.ast, dparse.parser, dparse.rollback_allocator;
import
    iz.memory;

enum ErrorType: ubyte
{
    warning,
    error
}

@NoInit @NoGc struct AstError
{
    ErrorType type;
    @NoGc string message;
    size_t line, column;

    @disable this();

    this(ErrorType type, string message, size_t line, size_t column) @nogc @safe
    {
        this.type = type;
        this.message = message;
        this.line = line;
        this.column = column;
    }
}

alias AstErrors = AstError*[];

@nogc @safe unittest
{
    AstError* err = construct!(AstError)(ErrorType.warning, "warning", 0, 0);
    assert(err);
    assert(err.type == ErrorType.warning);
    assert(err.message == "warning");
    assert(err.column == 0);
    assert(err.line == 0);
    destruct(err);
}

enum logCall =
q{
    import std.experimental.logger: log;
    version(devel) log();
    else version(unittest) log();
};

// TODO: define accurately the list of bad versions
version(linux)
    enum badVersions = ["none", "Windows", "Win32", "Win64", "OSX"];
else version(Windows)
    enum badVersions = ["none", "linux", "OSX", "Posix",
        "FreeBSD", "Solaris"];
else version(OSX)
    enum badVersions = ["none", "linux", "Windows", "Win32", "Win64"];
else static assert(0);

/**
 * Make a D string compatible with an Object Pascal string literal.
 */
string patchPascalString(string value)
{
    Appender!string app;
    app.reserve(value.length);
    bool skip;
    foreach (immutable i; 0..value.length)
    {
        char c = value[i];
        if (c > 0x7F)
        {
            app ~= value[i];
            skip = true;
        }
        else if (c == '\'')
        {
            if (skip)
                app ~= value[i];
            else
                app ~= "'#39'";
            skip = false;
        }
        else
        {
            app ~= value[i];
            skip = false;
        }
    }
    return app.data;
}

/// Used to annotate a public field that is written in `pascalStreaming()`.
enum Pascal;

/**
 * Streams a class or a struct using the Object Pascal streaming format, as defined
 * in FPC's FCL or Delphi's RTL.
 */
void pascalStreaming(T, string[][] enumLuts = [[""]], bool bin = false)(auto ref T t,
    ref Appender!string stream)
if (is(T == struct) || is(T == class))
{

    // TODO: find speification of the Pascal binary format
    static if (bin) {}
    else
    {
        stream ~= "object ";
        stream ~= T.stringof;
        stream ~= "\r";
    }

    foreach(member; __traits(allMembers, T))
    {
        static if (is(typeof(__traits(getMember, t, member))) &&
            hasUDA!(__traits(getMember, t, member), Pascal))
        {
            alias MT = typeof(__traits(getMember, t, member));
            alias TestBasicTypes = templateOr!(isSomeString, isIntegral,
                isFloatingPoint, isBoolean,);

            static if (is(MT == class) || is(MT == struct))
            {
                pascalStreaming!(MT, enumLuts, bin)(__traits(getMember, t, member), stream);
            }
            else static if (is(MT == enum))
            {
                import std.range: iota;
                bool done;
                static if (isIntegral!(OriginalType!MT))
                    foreach (i; aliasSeqOf!(iota(0,enumLuts.length)))
                {
                    static if (enumLuts[i].length == MT.max+1)
                    {
                        static if (bin) {}
                        else
                        {
                            stream ~= member;
                            stream ~= " = ";
                            stream ~= enumLuts[i][__traits(getMember, t, member)];
                            stream ~= "\r";
                        }
                        done = true;
                        break;
                    }
                }
                if (!done)
                {
                    static if (bin) {}
                    else
                    {
                        stream ~= member;
                        stream ~= " = ";
                        static if (isSomeString!MT)
                            stream ~= "'";
                        stream ~= to!string(__traits(getMember, t, member));
                        static if (isSomeString!MT)
                            stream ~= "'";
                        stream ~= "\r";
                    }
                }
            }
            else static if (TestBasicTypes!MT)
            {
                static if (bin) {}
                else
                {
                    stream ~= member;
                    stream ~= " = ";
                    stream ~= to!string(__traits(getMember, t, member));
                    stream ~= "\r";
                }
            }
            else static assert(0);
        }
    }

    static if (bin) {}
    else
    {
        stream ~= "end\r";
    }
}

unittest
{
    enum Bar{bar}

    static struct TRat
    {
        int notaprop1 = 0;
        @Pascal ubyte subProperty1 = 1;
        @Pascal string subProperty2 = "pascal";
    }

    static class TFoo
    {
        int notaprop1 = 0;
        @Pascal ubyte property1 = 1;
        @Pascal string property2 = "pascal";
        @Pascal Bar property3 = Bar.bar;
        @Pascal TRat rat;
    }

    Appender!string stream;
    pascalStreaming!(TFoo, [["bar"]])(new TFoo, stream);
    assert(stream.data != "");
}

/**
 * Produces and visits the AST for a source code.
 *
 * This function is used to handle the content of a MixinExpression in an
 * ASTVisitor.
 */
T parseAndVisit(T : ASTVisitor)(const(char)[] source)
{
    RollbackAllocator allocator;
    LexerConfig config = LexerConfig("", StringBehavior.source, WhitespaceBehavior.skip);
    StringCache cache = StringCache(StringCache.defaultBucketCount);
    const(Token)[] tokens = getTokensForParser(cast(ubyte[]) source, config, &cache);
    Module mod = parseModule(tokens, "", &allocator);
    T result = construct!(T);
    result.visit(mod);
    return result;
}


/**
Usage
=====

- In Coedit:
  the program must be located somewhere in the PATH.

- Elsewhere:
  invoke with `[-j] [<filename>]`.
  - `-j`: optional, if set then the program outputs the list (in stdout) in JSON
     otherwise in Pascal streaming text format.
  - `<filename>`: optional, the D module filename, if not set then the program
    reads the module from stdin.
  - see the source for more information about how to use the output.
    It's basically a tree of struct with 3 members: symbol type, name and location

- Test in CE as a runnable module:
  click `Compile file and run ...` and type either `<CFF>` or `-j <CFF>` in the
  input query dialog. Note that this will only work if libdparse is setup in the
  library manager.

*/
module cesyms;

import std.stdio, std.path, std.file, std.array, std.string, std.algorithm;
import std.getopt, std.json, std.conv, std.format;
import dparse.lexer, dparse.ast, dparse.parser, dparse.rollback_allocator;
import std.traits;


enum ListFmt
{
    Pas,
    Json
}

__gshared bool deep;

void main(string[] args)
{
    // format
    bool asJson;
    getopt(args, config.passThrough, "j", &asJson, "d", &deep);

    // get either the module from stdin or from first arg
    string fname;
    ubyte[] source;
    if (args.length == 1)
    {
        version (runnable_module)
        {
            source = cast(ubyte[]) read(__FILE__, size_t.max);
        }
        else
            foreach (buff; stdin.byChunk(1024))
                source ~= buff;
    }
    else if (args.length == 2)
    {
        fname = args[$ - 1];
        if (!fname.exists)
            return;
        source = cast(ubyte[]) read(fname, size_t.max);
    }
    else
        return;

    // parses, visits and writes to stdout
    RollbackAllocator alloc;
    auto config = LexerConfig(fname, StringBehavior.source, WhitespaceBehavior.skip);
    auto scache = StringCache(StringCache.defaultBucketCount);

    if (!asJson)
    {
        SymbolListBuilder!(ListFmt.Pas) slb = construct!(SymbolListBuilder!(ListFmt.Pas));
        auto ast = parseModule(getTokensForParser(source, config, &scache), fname,
            &alloc, &slb.astError);
        slb.visit(ast);
        write(slb.serialize);
        slb.destruct;
    }
    else
    {
        SymbolListBuilder!(ListFmt.Json) slb = construct!(SymbolListBuilder!(ListFmt.Json));
        auto ast = parseModule(getTokensForParser(source, config, &scache), fname,
            &alloc, &slb.astError);
        slb.visit(ast);
        write(slb.serialize);
        slb.destruct;
    }
}

// libdparse warnings includes some "'", which in Pascal are string delim
string patchPasStringLitteral(string p)
{
    import std.range : empty, front, popFront;

    string result;
    while (!p.empty)
    {
        dchar curr = p.front;
        switch (curr)
        {
        default:
            result ~= curr;
            break;
        case 10, 13:
            result ~= ' ';
            break;
        case '\'':
            result ~= "'#39'";
        }
        p.popFront;
    }
    return result;
}

// Memory utils ---------------------------------------------------------------+
void* getMem(size_t size) nothrow
{
    import std.c.stdlib: malloc;
    auto result = malloc(size);
    assert(result, "Out of memory");
    return result;
}

CT construct(CT, A...)(A a)
if (is(CT == class) && !isAbstractClass!CT)
{
    auto size = typeid(CT).init.length;
    auto memory = getMem(size);
    memory[0 .. size] = typeid(CT).init[];
    static if (__traits(hasMember, CT, "__ctor"))
        (cast(CT)(memory)).__ctor(a);
    import core.memory : GC;

    GC.addRange(memory, size, typeid(CT));
    return cast(CT) memory;
}

void destruct(T)(ref T instance)
if (is(T == class))
{
    if (!instance)
        return;
    destroy(instance);
    instance = null;
}
//----

enum SymbolType
{
    _alias,
    _class,
    _enum,
    _error,
    _function,
    _interface,
    _import,
    _mixin, // (template decl)
    _struct,
    _template,
    _union,
    _unittest,
    _variable,
    _warning
}

// AST visitor/Symbol list ----------------------------------------------------+
class SymbolListBuilder(ListFmt Fmt): ASTVisitor
{

    static if (Fmt == ListFmt.Pas)
    {
        static Appender!string pasStream;
    }
    else
    {
        static JSONValue json;
        static JSONValue* jarray;
    }

    static uint utc;

    alias visit = ASTVisitor.visit;

    static this()
    {
        static if (Fmt == ListFmt.Pas)
        {
            pasStream.put("object TSymbolList\rsymbols = <");
        }
        else
        {
            json = parseJSON("[]");
            jarray = &json;
        }
    }

    static void astError(string fname, size_t line, size_t col, string msg, bool isErr)
    {
        SymbolType type = isErr ? SymbolType._error : SymbolType._warning;
        static if (Fmt == ListFmt.Pas)
        {
            pasStream.put("\ritem\r");
            pasStream.put(format("line = %d\r", line));
            pasStream.put(format("col = %d\r", col));
            pasStream.put(format("name = '%s'\r", patchPasStringLitteral(msg)));
            pasStream.put(format("symType = %s\r", type));
            pasStream.put("end");
        }
        else
        {
            JSONValue item = parseJSON("{}");
            item["line"] = JSONValue(line);
            item["col"]  = JSONValue(col);
            item["name"] = JSONValue(msg);
            item["type"] = JSONValue(to!string(type));
            jarray.array ~= item;
        }
    }

    final string serialize()
    {
        static if (Fmt == ListFmt.Pas)
        {
            pasStream.put(">\rend\r\n");
            return pasStream.data;
        }
        else
        {
            JSONValue result = parseJSON("{}");
            result["items"] = json;
            version (assert)
                return result.toPrettyString;
            else
                return result.toString;
        }
    }

    /// visitor implementation if the declaration has a "name".
    final void namedVisitorImpl(DT, SymbolType st, bool dig = true)(const(DT) dt)
    if (__traits(hasMember, DT, "name"))
    {
        static if (Fmt == ListFmt.Pas)
        {
            pasStream.put("\ritem\r");
            pasStream.put(format("line = %d\r", dt.name.line));
            pasStream.put(format("col = %d\r", dt.name.column));
            pasStream.put(format("name = '%s'\r", dt.name.text));
            pasStream.put(format("symType = %s\r", st));
            static if (dig) if (deep)
            {
                pasStream.put("subs = <");
                dt.accept(this);
                pasStream.put(">\r");
            }
            pasStream.put("end");
        }
        else
        {
            JSONValue item = parseJSON("{}");
            item["line"] = JSONValue(dt.name.line);
            item["col"]  = JSONValue(dt.name.column);
            item["name"] = JSONValue(dt.name.text);
            item["type"] = JSONValue(to!string(st));
            static if (dig) if (deep)
            {
                JSONValue subs = parseJSON("[]");
                JSONValue* old = jarray;
                jarray = &subs;
                dt.accept(this);
                item["items"] = subs;
                jarray = old;
            }
            json.array ~= item;
        }
    }

    /// visitor implementation for special cases.
    final void otherVisitorImpl(DT, bool dig = true)
        (const(DT) dt, SymbolType st, string name, size_t line, size_t col)
    {
        static if (Fmt == ListFmt.Pas)
        {
            pasStream.put("\ritem\r");
            pasStream.put(format("line = %d\r", line));
            pasStream.put(format("col = %d\r", col));
            pasStream.put(format("name = '%s'\r", name));
            pasStream.put(format("symType = %s\r", st));
            static if (dig)
            {
                pasStream.put("subs = <");
                dt.accept(this);
                pasStream.put(">\r");
            }
            pasStream.put("end");
        }
        else
        {
            JSONValue item = parseJSON("{}");
            item["line"] = JSONValue(line);
            item["col"]  = JSONValue(col);
            item["name"] = JSONValue(name);
            item["type"] = JSONValue(to!string(st));
            static if (dig)
            {
                JSONValue subs = parseJSON("[]");
                JSONValue* old = jarray;
                jarray = &subs;
                dt.accept(this);
                item["items"] = subs;
                jarray = old;
            }
            json.array ~= item;
        }
    }

    final override void visit(const AliasDeclaration decl)
    {
        if (decl.initializers.length)
            namedVisitorImpl!(AliasInitializer, SymbolType._alias)(decl.initializers[0]);
    }

    final override void visit(const AnonymousEnumMember decl)
    {
        namedVisitorImpl!(AnonymousEnumMember, SymbolType._enum)(decl);
    }

    final override void visit(const AnonymousEnumDeclaration decl)
    {
        decl.accept(this);
    }

    final override void visit(const AutoDeclaration decl)
    {
        if (decl.identifiers.length)
        {
            otherVisitorImpl(decl, SymbolType._variable, decl.identifiers[0].text,
                decl.identifiers[0].line, decl.identifiers[0].column);
        }
    }

    final override void visit(const ClassDeclaration decl)
    {
        namedVisitorImpl!(ClassDeclaration, SymbolType._class)(decl);
    }

    final override void visit(const Constructor decl)
    {
        otherVisitorImpl(decl, SymbolType._function, "ctor", decl.line, decl.column);
    }

    final override void visit(const Destructor decl)
    {
        otherVisitorImpl(decl, SymbolType._function, "dtor", decl.line, decl.column);
    }

    final override void visit(const EnumDeclaration decl)
    {
        namedVisitorImpl!(EnumDeclaration, SymbolType._enum)(decl);
    }

    final override void visit(const EponymousTemplateDeclaration decl)
    {
        namedVisitorImpl!(EponymousTemplateDeclaration, SymbolType._template)(decl);
    }

    final override void visit(const FunctionDeclaration decl)
    {
        namedVisitorImpl!(FunctionDeclaration, SymbolType._function)(decl);
    }

    final override void visit(const InterfaceDeclaration decl)
    {
        namedVisitorImpl!(InterfaceDeclaration, SymbolType._interface)(decl);
    }

    final override void visit(const ImportDeclaration decl)
    {
        foreach (const(SingleImport) si; decl.singleImports)
        {
            if (!si.identifierChain.identifiers.length)
                continue;

            otherVisitorImpl(decl, SymbolType._import,
                si.identifierChain.identifiers.map!(a => a.text).join("."),
                si.identifierChain.identifiers[0].line,
                si.identifierChain.identifiers[0].column);
        }
        if (decl.importBindings) with (decl.importBindings.singleImport)
            otherVisitorImpl(decl, SymbolType._import,
                identifierChain.identifiers.map!(a => a.text).join("."),
                identifierChain.identifiers[0].line,
                identifierChain.identifiers[0].column);
    }

    final override void visit(const MixinTemplateDeclaration decl)
    {
        namedVisitorImpl!(TemplateDeclaration, SymbolType._mixin)(decl.templateDeclaration);
    }

    final override void visit(const StructDeclaration decl)
    {
        namedVisitorImpl!(StructDeclaration, SymbolType._struct)(decl);
    }

    final override void visit(const TemplateDeclaration decl)
    {
        namedVisitorImpl!(TemplateDeclaration, SymbolType._template)(decl);
    }

    final override void visit(const UnionDeclaration decl)
    {
        namedVisitorImpl!(UnionDeclaration, SymbolType._union)(decl);
    }

    final override void visit(const Unittest decl)
    {
        otherVisitorImpl(decl, SymbolType._unittest, format("test%.4d",utc++),
            decl.line, decl.column);
    }

    final override void visit(const VariableDeclaration decl)
    {
        if (decl.declarators)
            foreach (elem; decl.declarators)
                namedVisitorImpl!(Declarator, SymbolType._variable, false)(elem);
        else if (decl.autoDeclaration)
            visit(decl.autoDeclaration);
    }

    final override void visit(const StaticConstructor decl)
    {
        otherVisitorImpl(decl, SymbolType._function, "static ctor", decl.line, decl.column);
    }

    final override void visit(const StaticDestructor decl)
    {
        otherVisitorImpl(decl, SymbolType._function, "static dtor", decl.line, decl.column);
    }

    final override void visit(const SharedStaticConstructor decl)
    {
        otherVisitorImpl(decl, SymbolType._function, "shared static ctor", decl.line, decl.column);
    }

    final override void visit(const SharedStaticDestructor decl)
    {
        otherVisitorImpl(decl, SymbolType._function, "shared static dtor", decl.line, decl.column);
    }
}
//----


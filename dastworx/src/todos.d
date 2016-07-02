module todos;

import
    std.stdio, std.string, std.algorithm, std.array, std.conv, std.traits,
    std.ascii, std.range;
import
    dparse.lexer;
import
    common;

private __gshared Appender!string stream;

//TODO: sdfsfd

void getTodos(string[] files)
{
    mixin(logCall);
    //stream.reserve(2^^16);
    stream.put("object TTodoItems\r items = <");
    foreach(fname; files)
    {
        ubyte[] source;
        StringCache cache = StringCache(StringCache.defaultBucketCount);
        LexerConfig config = LexerConfig(fname, StringBehavior.source);
        File f = File(fname, "r");
        foreach (buffer; f.byChunk(4096))
            source ~= buffer;
        f.close;
        foreach(token; DLexer(source, config, &cache).array
            .filter!((a) => a.type == tok!"comment"))
                analyze(token, fname);
    }
    stream.put(">\rend\r\n");
    writeln(stream.data);
}

private void analyze(const(Token) token, string fname)
{
    string text = token.text.strip.patchPascalString;
    string identifier;

    mixin(logCall);

    // always comment
    text.popFrontN(2);
    if (text.empty)
        return;
    // ddoc suffix
    if (text.front.among('/', '*', '+'))
    {
        text.popFront;
        if (text.empty)
            return;
    }
    // leading whites
    while (text.front.isWhite)
    {
        text.popFront;
        if (text.empty)
            return;
    }

    // "TODO|FIXME|NOTE"
    bool isTodoComment;
    while (!text.empty)
    {
        identifier ~= std.ascii.toUpper(text.front);
        text.popFront;
        if (identifier.among("TODO", "FIXME", "NOTE"))
        {
            isTodoComment = true;
            break;
        }
    }
    if (!isTodoComment) return;
    identifier = "";

    // splits "fields" and "description"
    bool isWellFormed;
    string fields;
    while (!text.empty)
    {
        auto front = text.front;
        identifier ~= front;
        text.popFront;
        if (front == ':')
        {
            if (identifier.length) fields = identifier;
            isWellFormed = text.length > 0;
            break;
        }
    }
    if (!isWellFormed) return;
    identifier = "";

    // parses "fields"
    string a, c, p, s;
    while (!fields.empty)
    {
        const dchar front = fields.front;
        fields.popFront;
        if ((front == '-' || fields.empty) && identifier.length > 2)
        {
            string fieldContent = identifier[2..$].strip;
            switch(identifier[0..2].toUpper)
            {
                default: break;
                case "-A": a = fieldContent; break;
                case "-C": c = fieldContent; break;
                case "-P": p = fieldContent; break;
                case "-S": s = fieldContent; break;
            }
            identifier = "";
        }
        identifier ~= front;
    }

    if (text.length > 1 && text[$-2..$].among("*/", "+/"))
        text.length -=2;



    stream.put("\r item\r");
    stream.put(format("filename = '%s'\r", fname));
    stream.put(format("line = '%s'\r", token.line));
    stream.put(format("text = '%s'\r", text));
    if (c.length)
        stream.put(format("category = '%s'\r", c));
    if (a.length)
        stream.put(format("assignee = '%s'\r", a));
    if (p.length)
        stream.put(format("priority = '%s'\r", p));
    if (s.length)
        stream.put(format("status = '%s'\r", s));
    stream.put("end");
}

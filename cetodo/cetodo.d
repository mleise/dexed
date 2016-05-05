/*******************************************************************************
TODO source code analyzer for Coedit projects/files

## Format: 

`` 
[D comment prefix] TODO|FIXME [fields] : description
``

- D comment prefix: todo comments are detected in all the D comments kind.
In multi line comments, new lines must not be prefixed with '*' or '+'.
For example the following multiline comment is not suitable for a TODO comment:

/++
 + TODO:whatever.
 +/

but this one is:

/++
  TODO:whatever.
 +/

- TODO|FIXME: used to detect that the comment is a "TODO" comment.
The keywords are not case sensitive.

- fields: an optional list of properties with the format
`-<char x><property for char x>
the possible fields are:
    - c: TODO category, e.g: _-cserialization_, -cerrorhandling_.
    - a: TODO assignee, e.g: _-aMisterFreeze_, _-aFantomas_.
    - p: TODO priority, as an integer literal, eg: _-p8_, _-p0_.
    - s: TODO status, e.g _-sPartiallyFixed_, _-sDone_.
  
- description: what's to be done, e.g:  "set this property as const()".

## Examples:

``// TODO: set this property as const() to make it read-only.``

``// TODO-cfeature: save this property in the inifile.``

``// TODO-cannnotations-p8: annotate the members of the module with @safe and if possible nothrow.``

``// FIXME-p8: This won't work if all the flags are OR-ed.``

## Widget-to-tool IPC:

The widget calls the tool with a file list as argument and reads the process
output on exit. The widget expects to find some _TODO items_ in _LFM_ format, 
according to the classes declarations of TTodoItems (the collection container) 
and TTodoItem(the collection item).

********************************************************************************/
module cetodo;

import std.stdio, std.getopt, std.string, std.algorithm;
import std.array, std.conv, std.traits, std.ascii;
import std.file, std.path, std.range;
import dparse.lexer;

/// Encapsulates the fields of a TODO comment_.
private struct TodoItem 
{
    /** 
     * Enumerates the possible fields of _a TODO comment_. 
     * They must match the published member of the widget-side class TTodoItem.
     */
    private enum TodoField: ubyte {filename, line, text, category, assignee, priority, status}
    private __gshared static string[TodoField] fFieldNames;
    private string[TodoField.max+1] fFields;
    
    static this()
    {
        foreach(member; EnumMembers!TodoField)
            fFieldNames[member] = to!string(member);
    }
     
    /**
     * Constructs a TODO item with its fields.
     * Params:
     * fname = the file where the _TODO comment is located. mandatory.
     * line  = the line where the _TODO comment_is located. mandatory.
     * text  = the _TODO comment main text. mandatory.
     * cat   = the _TODO comment category, optional.
     * ass   = the _TODO comment assignee, optional.
     * prior = the _TODO comment priority, as an integer litteral, optional.
     * status= the _TODO comment status, optional.
     */
    @safe this(string fname, string line, string text, string cat = "",
        string ass = "", string prior = "", string status = "")
    {
        // priority must be convertible to int
        if (prior.length) try to!long(prior);
        catch(Exception e) prior = "";

        // Pascal strings are not multi-line
        version(Windows) immutable glue = "'#13#10'";
        else immutable glue = "'#10'";
        text = text.splitLines.join(glue);
              
        fFields[TodoField.filename] = fname;
        fFields[TodoField.line]     = line;
        fFields[TodoField.text]     = text;
        fFields[TodoField.category] = cat;
        fFields[TodoField.assignee] = ass;
        fFields[TodoField.priority] = prior;
        fFields[TodoField.status]   = status;
    }
    
    /**
     * The item writes itself as a TCollectionItem.
     * Params:
     * LfmString = the string containing the LFM script.
     */
    void serialize(ref Appender!string lfmApp)
    {
        lfmApp.put("  \r    item\r");
        foreach(member; EnumMembers!TodoField)
            if (fFields[member].length)
                lfmApp.put(format("      %s = '%s'\r", fFieldNames[member], fFields[member]));   
        lfmApp.put("    end");
    }
}

private alias TodoItems = TodoItem* [];

/**
 * Application main procedure.
 * Params:
 * args = a list of files to analyze. 
 * Called each time a document is focused. Args is set using:
 * - the symbolic string `<CFF>` (current file is not in a project).
 * - the symbolic string `<CPFS>` (current file is in a project).
 */
void main(string[] args)
{
    string[] files = args[1..$];
    Appender!string lfmApp;
    TodoItems todoItems;
    
    // helper to test in Coedit with Compile file & run.
    version(runnable_module)
    {
        if (!files.length)
            files ~= __FILE__;
    }
   
    foreach(f; files)
    {
        if (!f.exists) continue;
        
        // load and parse the file
        auto src = cast(ubyte[]) read(f, size_t.max);
        auto config = LexerConfig(f, StringBehavior.source);
        StringCache cache = StringCache(StringCache.defaultBucketCount);
        auto lexer = DLexer(src, config, &cache);
        
        // analyze the tokens
        foreach(tok; lexer) token2TodoItem(tok, f, todoItems);                     
    }
    
    // efficient appending if the item text ~ fields is about 100 chars
    lfmApp.reserve(todoItems.length * 128 + 64);

    // serialize the items using the pascal component streaming text format
    lfmApp.put("object TTodoItems\r  items = <");
    foreach(todoItem; todoItems) todoItem.serialize(lfmApp);
    lfmApp.put(">\rend\r\n");

    // the widget has the LFM script in the output
    write(lfmApp.data);
}

// comments may include some "'", which in Pascal are string delim
string patchPasStringLitteral(string str) @safe
{
    Appender!string app;
    app.reserve(str.length);
    bool skip;
    foreach (immutable i; 0..str.length)
    {
        char c = str[i];
        if (c > 0x7F)
        {
            app ~= str[i];
            skip = true;
        }
        else if (c == '\'')
        {
            if (skip)
                app ~= str[i];
            else
                app ~= "'#39'";
            skip = false;
        }
        else
        {
            app ~= str[i];
            skip = false;
        }
    }
    return app.data;
}

/// Try to transforms a Token into a a TODO item
@safe private void token2TodoItem(const(Token) atok, string fname, ref TodoItems todoItems)
{
    if (atok.type != (tok!"comment")) return;
    string text = atok.text.strip.patchPasStringLitteral;
    string identifier;

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

    // "TODO|FIXME"
    bool isTodoComment;
    while (!text.empty)
    {
        identifier ~= std.ascii.toUpper(text.front);
        text.popFront;   
        if (identifier.among("TODO","FIXME"))
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


    string line;
    try line = to!string(atok.line);
    catch(ConvException e) line = "0";
    todoItems ~= new TodoItem(fname, line, text, c, a, p, s);
}

// samples for testing the program as a runnable ('Compile file and run ...') with '<CFF>'

// fixme-p8: èuèuuè``u`èuùè é ^çßßðđææ«€¶ 
// fixme-p8: fixme also handled
// TODO-cINVALID_because_no_content:
/// TODO:set this property as const() to set it read only.
// TODO-cfeature-sDone:save this property in the inifile.
// TODO-cannnotations-p8: annotates the member of the module as @safe and if possible nothrow.
// TODO-cfeature-sDone: save this property in the inifile.
// TODO-aMe-cCat-p1-sjkjkj:todo body
/**
 TODO-cd:
 - this
 - that
*/
/++ TODO-cx:a mqkjfmksmldkf
+/
// TODO: it's fixed or it's not (issue #40) [çéèà]


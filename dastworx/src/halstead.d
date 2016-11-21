module halstead;

import
    std.meta, std.algorithm.iteration, std.json, std.conv;
import
    dparse.lexer, dparse.parser, dparse.ast, dparse.rollback_allocator;
import
    iz.memory, iz.containers;
version(unittest){} else import
    common;

/**
 * Retrieves the count and unique count of the operands and operators of
 * each function (inc. methods) of a module. After the call the results are
 * serialized as JSON in te standard output.
 */
void performHalsteadMetrics(const(Module) mod)
{
    HalsteadMetric hm = construct!(HalsteadMetric);
    hm.visit(mod);
    hm.serialize;
}

private struct Function
{
    size_t line;
    string name;
    size_t N1, n1;
    size_t N2, n2;
    alias operatorsSum = N1;
    alias operatorsKinds = n1;
    alias operandsSum = N2;
    alias operandsKinds = n2;
}

private struct BinaryExprFlags
{
    bool leftIsFunction;
    bool rightIsFunction;
}

private final class HalsteadMetric: ASTVisitor
{
    alias visit = ASTVisitor.visit;

    Function[] functions;
    size_t[string] operators;
    size_t[string] operands;
    BinaryExprFlags[] binExprFlag;
    size_t functionNesting;
    bool functionCall;
    bool ifStatement;
    JSONValue fs;

    void pushExprFlags(bool leftFlag = false, bool rightFlag = false)
    {
        binExprFlag.length += 1;
        binExprFlag[$-1].leftIsFunction = leftFlag;
        binExprFlag[$-1].rightIsFunction = rightFlag;
    }

    void popExprFlags()
    {
        binExprFlag.length -= 1;
    }

    bool exprLeftIsFunction(){return binExprFlag[$-1].leftIsFunction;}

    bool exprRightIsFunction(){return binExprFlag[$-1].rightIsFunction;}

    this()
    {
        fs = parseJSON("[]");
        pushExprFlags;
    }

    void serialize()
    {
        import std.stdio: write;
        JSONValue js;
        js["functions"] = fs;
        js.toString.write;
    }

    override void visit(const(PragmaExpression)){}
    override void visit(const(Unittest)){}

    void beginFunction()
    {
        operators.clear;
        operands.clear;
        if (functionNesting++ == 0)
            functions.length = functions.length + 1;
    }

    void endFunction(string name, size_t line)
    {
        functions[$-1].name = name;
        functions[$-1].line = line;

        if (operators.length)
        {
            functions[$-1].N1 = operators.byValue.fold!((a,b) => b = a + b);
            functions[$-1].n1 = operators.length;
        }
        if (operands.length)
        {
            functions[$-1].N2 = operands.byValue.fold!((a,b) => b = a + b);
            functions[$-1].n2 = operands.length;
        }

        JSONValue f;
        f["name"] = functions[$-1].name;
        f["line"] = functions[$-1].line;
        f["n1Sum"] = functions[$-1].N1;
        f["n1Count"] = functions[$-1].n1;
        f["n2Sum"] = functions[$-1].N2;
        f["n2Count"] = functions[$-1].n2;
        fs ~= [f];

        version(unittest)
        {
            import std.stdio: writeln;
            writeln(functions[$-1]);
            writeln('\t',operators);
            writeln('\t',operands);
        }

        functionNesting--;
    }

    override void visit(const(FunctionCallExpression) expr)
    {
        if (expr.unaryExpression.primaryExpression)
        {
            const(PrimaryExpression) p = expr.unaryExpression.primaryExpression;
            if (p.identifierOrTemplateInstance)
            {
                if (p.identifierOrTemplateInstance.templateInstance)
                    ++operators[p.identifierOrTemplateInstance.templateInstance.identifier.text];
                else
                    ++operators[p.identifierOrTemplateInstance.identifier.text];
            }
        }
        if (expr.templateArguments)
        {
            if (expr.templateArguments.templateSingleArgument)
                ++operands[expr.templateArguments.templateSingleArgument.token.text];
            else if (expr.templateArguments.templateArgumentList)
            {
                foreach(arg; expr.templateArguments.templateArgumentList.items)
                    {}//++operands[arg.token.text];
            }
        }
        expr.accept(this);
    }

    override void visit(const(FunctionDeclaration) decl)
    {
        beginFunction;
        if (!decl.functionBody)
            return;
        decl.accept(this);
        endFunction(decl.name.text, decl.name.line);
    }

    override void visit(const(SharedStaticConstructor) ssc)
    {
        beginFunction;
        ssc.accept(this);
        endFunction("sharedStaticCtorL" ~ to!string(ssc.line), ssc.line);
    }

    override void visit(const(StaticConstructor) sc)
    {
        beginFunction;
        sc.accept(this);
        endFunction("staticCtorL" ~ to!string(sc.line), sc.line);
    }

    override void visit(const(Constructor) sc)
    {
        beginFunction;
        sc.accept(this);
        endFunction("ctorL" ~ to!string(sc.line), sc.line);
    }

    override void visit(const(SharedStaticDestructor) ssc)
    {
        beginFunction;
        ssc.accept(this);
        endFunction("sharedStaticDtorL" ~ to!string(ssc.line), ssc.line);
    }

    override void visit(const(StaticDestructor) sc)
    {
        beginFunction;
        sc.accept(this);
        endFunction("staticDtorL" ~ to!string(sc.line), sc.line);
    }

    override void visit(const(Destructor) sc)
    {
        beginFunction;
        sc.accept(this);
        endFunction("dtorL" ~ to!string(sc.line), sc.line);
    }

    override void visit(const(PrimaryExpression) primary)
    {
	    if (primary.identifierOrTemplateInstance !is null
		    && primary.identifierOrTemplateInstance.identifier != tok!"")
        {
            if (!functionCall ||  (functionCall & exprLeftIsFunction) || (functionCall & exprRightIsFunction))
                ++operands[primary.identifierOrTemplateInstance.identifier.text];
        }
        else if (primary.primary.type.isLiteral)
        {
            import std.digest.crc: crc32Of, toHexString;
            ++operands["literal" ~ primary.primary.text.crc32Of.toHexString.idup];
        }

        functionCall = false;

        primary.accept(this);
    }

    override void visit(const(UnaryExpression) expr)
    {
        if (expr.prefix.type)
            ++operators[str(expr.prefix.type)];
        if (expr.suffix.type)
            ++operators[str(expr.suffix.type)];

        if (expr.functionCallExpression)
            functionCall = true;

        expr.accept(this);
    }

    override void visit(const(IndexExpression) expr)
    {
        ++operators["[]"];
        expr.accept(this);
    }

    override void visit(const(NewExpression) expr)
    {
        ++operators["new"];
        expr.accept(this);
    }

    override void visit(const(NewAnonClassExpression) expr)
    {
        ++operators["new"];
        expr.accept(this);
    }

    override void visit(const(DeleteExpression) expr)
    {
        ++operators["delete"];
        expr.accept(this);
    }

    override void visit(const(CastExpression) expr)
    {
        ++operators["cast"];
        expr.accept(this);
    }

    override void visit(const(IsExpression) expr)
    {
        ++operators["is"];
        expr.accept(this);
    }

    override void visit(const(TypeidExpression) expr)
    {
        ++operators["typeid"];
        expr.accept(this);
    }

    override void visit(const(IfStatement) st)
    {
        ++operators["if"];
        st.accept(this);
        if (st.thenStatement)
            ++operators["then"];
        if (st.elseStatement)
            ++operators["else"];
    }

    override void visit(const(DeclarationOrStatement) st)
    {
        st.accept(this);
    }

    override void visit(const(WhileStatement) st)
    {
        ++operators["while"];
        st.accept(this);
    }

    override void visit(const(ForStatement) st)
    {
        ++operators["for"];
        st.accept(this);
    }

    override void visit(const(ForeachStatement) st)
    {
        ++operators["foreach"];
        if (st.foreachTypeList)
            foreach(ft; st.foreachTypeList.items)
                ++operands[ft.identifier.text];
        if (st.foreachType)
            ++operands[st.foreachType.identifier.text];
        st.accept(this);
    }

    override void visit(const(ReturnStatement) st)
    {
        ++operators["return"];
        st.accept(this);
    }

    override void visit(const(BreakStatement) st)
    {
        ++operators["break"];
        st.accept(this);
    }

    override void visit(const(ContinueStatement) st)
    {
        ++operators["continue"];
        st.accept(this);
    }

    override void visit(const(GotoStatement) st)
    {
        ++operators["goto"];
        st.accept(this);
    }

    override void visit(const(SwitchStatement) st)
    {
        ++operators["switch"];
        st.accept(this);
    }

    override void visit(const(CaseStatement) st)
    {
        ++operators["case"];
        st.accept(this);
    }

    override void visit(const(CaseRangeStatement) st)
    {
        ++++operators["case"];
        st.accept(this);
    }

    override void visit(const(DefaultStatement) st)
    {
        ++operators["case"];
        st.accept(this);
    }

    override void visit(const(ThrowStatement) st)
    {
        ++operators["throw"];
        st.accept(this);
    }

    override void visit(const(TryStatement) st)
    {
        ++operators["try"];
        st.accept(this);
    }

    override void visit(const(Catch) c)
    {
        ++operators["catch"];
        c.accept(this);
        if (c.identifier.text)
            ++operands[c.identifier.text];
    }

    override void visit(const(VariableDeclaration) decl)
    {
        if (decl.declarators)
            foreach (elem; decl.declarators)
            {
                ++operands[elem.name.text];
                if (elem.initializer)
                    ++operators["="];
            }
        else if (decl.autoDeclaration)
            visit(decl.autoDeclaration);
        decl.accept(this);
    }

    final override void visit(const AutoDeclarationPart decl)
    {
        ++operands[decl.identifier.text];
        ++operators["="];
        decl.accept(this);
    }

    void visitBinExpr(T)(const(T) expr)
    {
        bool leftArgIsFunctFlag;
        bool rightArgIsFunctFlag;
        static if (__traits(hasMember, T, "left"))
        {
            if (expr.left && (cast(UnaryExpression) expr.left) &&
                (cast(UnaryExpression) expr.left).functionCallExpression)
                    leftArgIsFunctFlag = true;
        }
        static if (__traits(hasMember, T, "right"))
        {
            if (expr.right && (cast(UnaryExpression) expr.right) &&
                (cast(UnaryExpression) expr.right).functionCallExpression)
                    rightArgIsFunctFlag = true;
        }

        string op;
        static if (__traits(hasMember, T, "operator"))
        {
            op = str(expr.operator);
        }
        else
        {
            static if (is(T == AndExpression)) op = `&`;
            else static if (is(T == AndAndExpression)) op = `&&`;
            else static if (is(T == AsmAndExp)) op = `&`;
            else static if (is(T == AsmLogAndExp)) op = "?";
            else static if (is(T == AsmLogOrExp)) op = "?";
            else static if (is(T == AsmOrExp)) op = "|";
            else static if (is(T == AsmXorExp)) op = "|";
            else static if (is(T == IdentityExpression)) op = expr.negated ? "!is" : "is";
            else static if (is(T == InExpression)) op = expr.negated ? "!in" : "in";
            else static if (is(T == OrExpression)) op = `|`;
            else static if (is(T == OrOrExpression)) op = `||`;
            else static if (is(T == PowExpression)) op = `^`;
            else static if (is(T == XorExpression)) op = `^^`;
            else static assert(0, T.stringof);
        }
        ++operators[op];

        pushExprFlags(leftArgIsFunctFlag, rightArgIsFunctFlag);
        expr.accept(this);
        popExprFlags;
    }

    static string binExprsString()
    {
        import std.range: iota;

        alias SeqOfBinExpr = AliasSeq!(
            AddExpression,
            AndExpression,
            AndAndExpression,
            AsmAddExp,
            AsmAndExp,
            AsmEqualExp,
            AsmLogAndExp,
            AsmLogOrExp,
            AsmMulExp,
            AsmOrExp,
            AsmRelExp,
            AsmShiftExp,
            AsmXorExp,
            AssignExpression,
            EqualExpression,
            IdentityExpression,
            InExpression,
            MulExpression,
            OrExpression,
            OrOrExpression,
            PowExpression,
            RelExpression,
            ShiftExpression,
            XorExpression,
        );

        enum binExpOverrideOverride(T) =
        "override void visit(const(" ~ T.stringof ~ ") expr)
        {
            visitBinExpr(expr);
        }";

        string result;
        foreach(i; aliasSeqOf!(iota(0, SeqOfBinExpr.length)))
            result ~= binExpOverrideOverride!(SeqOfBinExpr[i]);
        return result;
    }

    mixin(binExprsString());
}

version(unittest)
{
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

    Function test(string source)
    {
        HalsteadMetric hm = parseAndVisit!(HalsteadMetric)(source);
        scope(exit) destruct(hm);
        return hm.functions[$-1];
    }
}

unittest
{
    Function r =
    q{
        void foo()
        {
            Object o = new Object;
        }
    }.test;
    assert(r.operandsKinds == 1);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            auto o = new Object;
        }
    }.test;
    assert(r.operandsKinds == 1);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            auto o = 1 + 2;
        }
    }.test;
    assert(r.operandsKinds == 3);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            foo(bar,baz);
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            int i = foo(bar,baz) + foo(bar,baz);
        }
    }.test;
    assert(r.operandsKinds == 4);
    assert(r.operatorsKinds == 3);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            enum E{e0}
            E e;
            bar!(e,"lit")(baz(e));
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo();
    }.test;
    assert(r.operandsKinds == 0);
    assert(r.operatorsKinds == 0);
}

unittest
{
    Function r =
    q{
        shared static this()
        {
            int i = 0;
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        shared static ~this()
        {
            int i = 0;
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        static this()
        {
            int i = 0;
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        static ~this()
        {
            int i = 0;
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        class Foo
        {
            this()
            {
                int i = 0;
            }
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        class Foo
        {
            ~this()
            {
                int i = 0;
            }
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            i += a << b;
        }
    }.test;
    assert(r.operandsKinds == 3);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            ++h;
            i--;
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            ++i--;
        }
    }.test;
    assert(r.operandsKinds == 1);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            i = a | b & c && d || e^f + g^^h - a in b + a[0];
        }
    }.test;
    assert(r.operandsKinds == 10);
    assert(r.operatorsKinds == 11);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            Bar bar = new Bar;
            auto baz = cast(Baz) bar;
            delete bar;
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            foreach(i,a;z){}
        }
    }.test;
    assert(r.operandsKinds == 3);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            foreach(i; l..h){}
        }
    }.test;
    assert(r.operandsKinds == 3);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            for(i = 0; i < len; i++){}
        }
    }.test;
    assert(r.operandsKinds == 3);
    assert(r.operatorsKinds == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            for(;;){continue;}
        }
    }.test;
    assert(r.operandsKinds == 0);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        int foo()
        {
            while(true) {return 0;}
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 2);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            switch(a)
            {
                default: break;
                case 1: return;
                case 2: .. case 8: ;
            }
        }
    }.test;
    assert(r.operandsKinds == 4);
    assert(r.operatorsKinds == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            try a();
            catch(Exception e)
                throw v;
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            if (true) {} else {i = 0;}
        }
    }.test;
    assert(r.operandsKinds == 3);
    assert(r.operatorsKinds == 4);
}

version(none) unittest
{
    // TODO: detect function call w/o parens
    Function r =
    q{
        void foo()
        {
            bar;
        }
    }.test;
    assert(r.operandsKinds == 0);
    assert(r.operatorsKinds == 1);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a = bar(b) + baz(z);
        }
    }.test;
    assert(r.operandsKinds == 5);
    assert(r.operatorsKinds == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a = bar(cat(0) - dog(1)) + baz(z);
        }
    }.test;
    assert(r.operandsKinds == 8);
    assert(r.operatorsKinds == 7);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a = bar(cat(0) && dog(1)) | baz(z);
        }
    }.test;
    assert(r.operandsKinds == 8);
    assert(r.operatorsKinds == 7);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a = bar(c)++;
        }
    }.test;
    // would be 3 by considering bar as an operand
    // but this is actually invalid code.
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 3);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a = !!!a;
        }
    }.test;
    assert(r.operandsKinds == 1);
    assert(r.operatorsKinds == 2);
    assert(r.operatorsSum == 4);
}

unittest
{
    Function r =
    q{
        void foo()
        {
            a = b[foo(a)];
        }
    }.test;
    assert(r.operandsKinds == 2);
    assert(r.operatorsKinds == 3);
}


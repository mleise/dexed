module halstead;

import
    std.meta, std.traits, std.algorithm.iteration, std.json, std.conv;
import
    dparse.lexer, dparse.parser, dparse.ast, dparse.rollback_allocator;
import
    iz.memory, iz.containers;
version(unittest){} else import
    common;

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
}

private final class HalsteadMetric: ASTVisitor
{
    alias visit = ASTVisitor.visit;

    Function[] functions;
    size_t[string] operators;
    size_t[string] operands;
    size_t functionNesting;
    bool functionCall;
    bool ifStatement;
    JSONValue fs;

    this()
    {
        fs = parseJSON("[]");
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
            import std.stdio;
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
        if (!decl.functionBody)
            return;

        beginFunction;
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
            if (!functionCall)
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

    override void visit(const(AndAndExpression) expr)
    {
        ++operators["&&"];
        expr.accept(this);
    }

    override void visit(const(OrOrExpression) expr)
    {
        ++operators["||"];
        expr.accept(this);
    }

    override void visit(const(AndExpression) expr)
    {
        ++operators["&"];
        expr.accept(this);
    }

    override void visit(const(AsmAndExp) expr)
    {
        ++operators["&"];
        expr.accept(this);
    }

    override void visit(const(OrExpression) expr)
    {
        ++operators["|"];
        expr.accept(this);
    }

    override void visit(const(InExpression) expr)
    {
        ++operators["in"];
        expr.accept(this);
    }

    override void visit(const(PowExpression) expr)
    {
        ++operators["^"];
        expr.accept(this);
    }

    override void visit(const(XorExpression) expr)
    {
        ++operators["^^"];
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
        ifStatement = true;
        st.accept(this);
        ifStatement = false;
    }

    override void visit(const(DeclarationOrStatement) st)
    {
        if (ifStatement && st.statement)
            ++operators["thenOrElse"];
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
        ++operators["case"];
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

    static string exprAliases()
    {
        import std.range: iota;

        alias ExprWithOp = AliasSeq!(
            AddExpression,
            AsmAddExp,
            AsmEqualExp,
            AsmMulExp,
            AsmRelExp,
            AsmShiftExp,
            AssignExpression,
            EqualExpression,
            MulExpression,
            RelExpression,
            ShiftExpression,
        );

        enum exprOverride(T) = "
        override void visit(const(" ~ T.stringof ~ ") expr)
        {
            static assert(__traits(hasMember," ~ T.stringof ~ ", \"operator\"));
            ++operators[str(expr.operator)];
            expr.accept(this);
        }";

        string result;
        foreach(i; aliasSeqOf!(iota(0, ExprWithOp.length)))
            result ~= exprOverride!(ExprWithOp[i]);
        return result;
    }

    mixin(exprAliases);

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

    unittest
    {
        string src =
        q{
            void foo()
            {
                Object o = new Object;
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 1);
        assert(r.operators.length == 2);
    }

    unittest
    {
        string src =
        q{
            void foo()
            {
                auto o = new Object;
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 1);
        assert(r.operators.length == 2);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            void foo()
            {
                auto o = 1 + 2;
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 3);
        assert(r.operators.length == 2);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            void foo()
            {
                foo(bar,baz);
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 2);
        assert(r.operators.length == 1);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            void foo()
            {
                int i = foo(bar,baz) + foo(bar,baz);
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 3);
        assert(r.operators.length == 3);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            void foo()
            {
                enum E{e0}
                E e;
                bar!(e,"lit")(baz(e));
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 2);
        assert(r.operators.length == 2);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            void foo();
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 0);
        assert(r.operators.length == 0);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            shared static this()
            {
                int i = 0;
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 2);
        assert(r.operators.length == 1);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            shared static ~this()
            {
                int i = 0;
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 2);
        assert(r.operators.length == 1);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            static this()
            {
                int i = 0;
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 2);
        assert(r.operators.length == 1);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            static ~this()
            {
                int i = 0;
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 2);
        assert(r.operators.length == 1);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            class Foo
            {
                this()
                {
                    int i = 0;
                }
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 2);
        assert(r.operators.length == 1);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            class Foo
            {
                ~this()
                {
                    int i = 0;
                }
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 2);
        assert(r.operators.length == 1);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            void foo()
            {
                i += a << b;
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 3);
        assert(r.operators.length == 2);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            void foo()
            {
                ++h;
                i--;
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 2);
        assert(r.operators.length == 2);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            void foo()
            {
                ++i--;
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 1);
        assert(r.operators.length == 2);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            void foo()
            {
                i = a | b & c && d || e^f + g^^h - a in b + a[0];
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 10);
        assert(r.operators.length == 11);
        r.destruct;
    }

    unittest
    {
        string src =
        q{
            void foo()
            {
                Bar bar = new Bar;
                auto baz = cast(Baz) bar;
                delete bar;
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        assert(r.operands.length == 2);
        assert(r.operators.length == 4);
        r.destruct;
    }

    unittest
    {
        // TODO: detect function call w/o parens
        string src =
        q{
            void foo()
            {
                bar;
            }
        };
        HalsteadMetric r = src.parseAndVisit!HalsteadMetric;
        //assert(r.operands.length == 0);
        //assert(r.operators.length == 1);
        r.destruct;
    }
}


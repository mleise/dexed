module halstead;

import
    std.meta, std.traits, std.algorithm.iteration, std.json;
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

    //TODO: add share/static/__ctor & __dtor

    override void visit(const(FunctionDeclaration) decl)
    {
        if (!decl.functionBody)
            return;

        if (functionNesting++ == 0)
            functions.length = functions.length + 1;

        decl.accept(this);

        functions[$-1].name = decl.name.text;
        functions[$-1].line = decl.name.line;

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

        operators.clear;
        operands.clear;

        functionNesting--;
    }

    override void visit(const(PrimaryExpression) primary)
    {
	    if (primary.identifierOrTemplateInstance !is null
		    && primary.identifierOrTemplateInstance.identifier != tok!"")
        {
            if (!functionCall)
                ++operands[primary.identifierOrTemplateInstance.identifier.text];
            else
                ++operators[primary.identifierOrTemplateInstance.identifier.text];

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

        // TODO: detect function name here
        if (expr.functionCallExpression)
            functionCall = true;

        // TODO: detect function call w/o parens
        //else if (expr.prefix.type == tok!"" && expr.suffix.type == tok!"")
        //    functionCall = true;

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

    void test(T)(T t)
    {

        auto a = 1;
        auto b = a++;
        auto c = a || b;
        auto d = a << c;
        auto e = a >>> c;
        test(test());
        test;
    }

    unittest
    {
        import std.file;
        char[] source = cast(char[]) __FILE__.read;
        auto r = source.parseAndVisit!HalsteadMetric;
    }
}


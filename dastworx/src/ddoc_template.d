module ddoc_template;

import
    std.stdio;
import
    iz.memory;
import
    dparse.ast, dparse.lexer, dparse.parser, dparse.rollback_allocator;

/**
 * Finds the declaration at caretLine and write its ddoc template
 * in the standard output.
 */
void getDdocTemplate(const(Module) mod, int caretLine)
{
    DDocTemplateGenerator dtg = construct!DDocTemplateGenerator(caretLine);
    dtg.visit(mod);
}

final class DDocTemplateGenerator: ASTVisitor
{
    alias visit = ASTVisitor.visit;

private:

    int _caretline;

public:

    this(int caretline)
    {
        _caretline = caretline;
    }

    override void visit(const(FunctionDeclaration) decl)
    {
        if (decl.name.line == _caretline)
        {
            writeln("/**\n * <short description> \n * \n * <detailed description>\n *");

            if (decl.templateParameters || decl.parameters)
            {
                writeln(" * Params:");

                if (decl.templateParameters && decl.templateParameters.templateParameterList)
                {
                    foreach(const TemplateParameter p;  decl.templateParameters
                                                            .templateParameterList.items)
                    {
                        if (p.templateAliasParameter)
                            writeln(" *     ", p.templateAliasParameter.identifier.text, " = <description>");
                        else if (p.templateTupleParameter)
                            writeln(" *     ", p.templateTupleParameter.identifier.text, " = <description>");
                        else if (p.templateTypeParameter)
                            writeln(" *     ", p.templateTypeParameter.identifier.text, " = <description>");
                        else if (p.templateValueParameter)
                            writeln(" *     ", p.templateValueParameter.identifier.text, " = <description>");
                    }
                }
                if (decl.parameters)
                {
                    foreach(i, const Parameter p; decl.parameters.parameters)
                    {
                        if (p.name.text != "")
                            writeln(" *     ", p.name.text, " = <description>");
                        else
                            writeln(" *     __param", i, " = <description>");
                    }
                }
            }

            if (decl.returnType)
            {
                if (decl.returnType.type2 && decl.returnType.type2
                    && decl.returnType.type2.builtinType != tok!"void")
                        writeln(" * \n * Returns: <return description>");
            }

            writeln(" */");

        }
        else if (decl.name.line > _caretline)
            return;
        decl.accept(this);
    }

    override void visit(const(TemplateDeclaration) decl)
    {
        visitTemplateOrAggregate(decl);
    }

    override void visit(const(ClassDeclaration) decl)
    {
        visitTemplateOrAggregate(decl);
    }

    override void visit(const(StructDeclaration) decl)
    {
        visitTemplateOrAggregate(decl);
    }

    override void visit(const(UnionDeclaration) decl)
    {
        visitTemplateOrAggregate(decl);
    }

    private void visitTemplateOrAggregate(T)(const(T) decl)
    {
        if (decl.name.line == _caretline)
        {
            writeln("/**\n * <short description> \n * \n * <detailed description>\n *");

            if (decl.templateParameters)
            {
                writeln(" * Params:");

                if (decl.templateParameters && decl.templateParameters.templateParameterList)
                {
                    foreach(const TemplateParameter p;  decl.templateParameters
                                                            .templateParameterList.items)
                    {
                        if (p.templateAliasParameter)
                            writeln(" *     ", p.templateAliasParameter.identifier.text, " = <description>");
                        else if (p.templateTupleParameter)
                            writeln(" *     ", p.templateTupleParameter.identifier.text, " = <description>");
                        else if (p.templateTypeParameter)
                            writeln(" *     ", p.templateTypeParameter.identifier.text, " = <description>");
                        else if (p.templateValueParameter)
                            writeln(" *     ", p.templateValueParameter.identifier.text, " = <description>");
                    }
                }

            }
            writeln(" */");

        }
        else if (decl.name.line > _caretline)
            return;
        decl.accept(this);
    }
}

version(unittest)
{
    DDocTemplateGenerator parseAndVisit(const(char)[] source, int caretLine)
    {
        writeln;
        RollbackAllocator allocator;
        LexerConfig config = LexerConfig("", StringBehavior.source, WhitespaceBehavior.skip);
        StringCache cache = StringCache(StringCache.defaultBucketCount);
        const(Token)[] tokens = getTokensForParser(cast(ubyte[]) source, config, &cache);
        Module mod = parseModule(tokens, "", &allocator);
        DDocTemplateGenerator result = construct!(DDocTemplateGenerator)(caretLine);
        result.visit(mod);
        return result;
    }
}

unittest
{
    q{ module a;
       void foo(A...)(A a){}
    }.parseAndVisit(2);
}

unittest
{
    q{ module a;
       void foo()(){}
    }.parseAndVisit(2);
}

unittest
{
    q{ module a;
       int foo(int){}
    }.parseAndVisit(2);
}

unittest
{
    q{ module a;
       class Foo(T, A...){}
    }.parseAndVisit(2);
}

unittest
{
    q{ module a;
       struct Foo(alias Fun, A...){}
    }.parseAndVisit(2);
}


module imports;

import
    std.stdio, std.algorithm, std.array;
import
    iz.memory;
import
    dparse.lexer, dparse.ast, dparse.parser;
import
    common;

/**
 * Lists the modules imported b y a module
 *
 * Each import is written in a new line. Import detection is not accurate,
 * the imports injected by a mixin template or by a string variable are not detected,
 * the imports deactivated by a static condition neither.
 *
 * The results are used by to detect which are the static libraries used by a
 * runnable module.
 */
void listImports(const(Module) mod)
in
{
    assert(mod);
}
body
{
    mixin(logCall);
    construct!(ImportLister).visit(mod);
}

private final class ImportLister: ASTVisitor
{
    alias visit = ASTVisitor.visit;
    size_t mixinDepth;

    override void visit(const ConditionalDeclaration decl)
    {
        const VersionCondition ver = decl.compileCondition.versionCondition;
        if (ver is null || !canFind(badVersions, ver.token.text))
            decl.accept(this);
    }

    override void visit(const(ImportDeclaration) decl)
    {
        foreach (const(SingleImport) si; decl.singleImports)
        {
            if (!si.identifierChain.identifiers.length)
                continue;
            si.identifierChain.identifiers.map!(a => a.text).join(".").writeln;
        }
        if (decl.importBindings) with (decl.importBindings.singleImport)
            identifierChain.identifiers.map!(a => a.text).join(".").writeln;
    }

    override void visit(const(MixinExpression) mix)
    {
        ++mixinDepth;
        mix.accept(this);
        --mixinDepth;
    }

    override void visit(const PrimaryExpression primary)
    {
        if (mixinDepth && primary.primary.type.isStringLiteral)
        {
            assert(primary.primary.text.length > 1);

            size_t startIndex = 1;
            startIndex += primary.primary.text[0] == 'q';
            parseAndVisit!(ImportLister)(primary.primary.text[startIndex..$-1]);
        }
        primary.accept(this);
    }
}


module mainfun;

import
    std.stdio, std.algorithm;
import
    iz.memory;
import
    dparse.lexer, dparse.ast, dparse.parser;
import
    common;

/**
 * Detects wether a main function is declared in a module.
 *
 * Writes "1" if a main is found otherwise "0". The detection is not accurate,
 * if the main is injected by a mixin template or by a string it is not detected,
 * if the main is deactivated by a static condition neither.
 *
 * The result is used by to detect if the "-main" switch has to be passed to
 * the compiler.
 */
void detectMainFun(const(Module) mod)
{
    mixin(logCall);
    MainFunctionDetector mfd = construct!(MainFunctionDetector);
    mfd.visit(mod);
    write(mfd.hasMain);
    stdout.flush;
}

private final class MainFunctionDetector: ASTVisitor
{
    alias visit = ASTVisitor.visit;

    ubyte hasMain;

    this()
    {
        hasMain = false;
    }

    override void visit(const ConditionalDeclaration decl)
    {
        const VersionCondition ver = decl.compileCondition.versionCondition;
        if (ver is null || !canFind(badVersions, ver.token.text))
            decl.accept(this);
    }

    override void visit(const(FunctionDeclaration) decl)
    {
        if (decl.name.text == "main")
            hasMain = true;
    }
}


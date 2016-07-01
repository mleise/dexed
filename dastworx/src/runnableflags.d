module runnableflags;

import
    std.stdio;
import
    dparse.lexer;
import
    common;

/**
 * Parse the compiler switch defined in the comments located before a
 * ModuleDeclaration and that are passed to the compiler when a runnable is
 * launched.
 *
 * each line of the soutput contains an option.
 */
void getRunnableFlags(const(Token)[] tokens)
{
    mixin(logCall);
}

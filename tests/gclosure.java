
package com.google.javascript.jscomp;

class UnreachableCodeElimination {
    private static final Logger logger =
        Logger.getLogger(UnreachableCodeElimination.class.getName());
    private final AbstractCompiler compiler;
    private boolean codeChanged;

  UnreachableCodeElimination(AbstractCompiler compiler) {
    this.compiler = compiler;
  }

    /**
     * @param contents The string representing the source map file contents.
     * @return The parsed source map.
     * @throws SourceMapParseException
     */
    @Override
    public static SourceMapping parse(String contents)
        throws SourceMapParseException {
        return parse(contents, null);
    }


}

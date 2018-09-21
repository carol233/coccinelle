
package com.google.javascript.jscomp;

class UnreachableCodeElimination {

  UnreachableCodeElimination(AbstractCompiler compiler) {
    this.compiler = compiler;
  }

    /**
     * @param contents The string representing the source map file contents.
     * @return The parsed source map.
     * @throws SourceMapParseException
     */
    public static SourceMapping parse(String contents)
        throws SourceMapParseException {
        return parse(contents, null);
    }


}

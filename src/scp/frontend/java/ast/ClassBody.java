package scp.frontend.java.ast;

import scp.frontend.java.lexer.Token;

/**
 * Contains all of the syntactic constructs in the body of a class
 * definition.
 */
public class ClassBody {
  ClassParts classParts;

  /**
   * Construct a new ClassBody with the specified ClassParts.
   */
  public ClassBody(ClassParts classParts) {
    this.classParts = classParts;
  }

  /**
   * Construct a new empty ClassBody.
   */
  public ClassBody() {
    this.classParts = new ClassParts();
  }
}

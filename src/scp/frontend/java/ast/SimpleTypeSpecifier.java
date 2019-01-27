package scp.frontend.java.ast;

import scp.frontend.java.lexer;

/**
 * Represents a type specifier which is a simple reference to an
 * identifier.
 */
public class SimpleTypeSpecifier extends TypeSpecifier {
  Token identifier;

  /**
   * Constructs a new instance based on the specified identifier for
   * the type.
   */
  public SimpleTypeSpecifier(Token identifier) {
    this.identifier = identifier;
  }
}

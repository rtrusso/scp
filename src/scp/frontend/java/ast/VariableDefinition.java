package scp.frontend.java.ast;

import scp.frontend.java.lexer.Token;

/**
 * Represents a syntactic variable definition, as it may occur in the
 * body of a method.  This object is indirectly used to represent
 * class fields through the FieldDefinition object.
 */
public class VariableDefinition {
  TypeSpecifier typeSpecifier;
  Token identifier;

  /**
   * Construct a new instance based on the type specifier and the
   * name.
   */
  public VariableDefinition(TypeSpecifier typeSpecifier, Token identifier) {
    this.typeSpecifier = typeSpecifier;
    this.identifier = identifier;
  }
}

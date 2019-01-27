package scp.frontend.java.ast;

import scp.frontend.java.lexer.Token;

/**
 * Represents a syntactic parameter definition.
 */
public class Parameter extends VariableDefinition {

  /**
   * Construct a new instance based on the type and identifier.
   */
  public Parameter(TypeSpecifier type, Token identifier) {
    super(type, identifier);
  }
}

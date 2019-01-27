package scp.frontend.java.ast;

import scp.frontend.java.lexer.Token;

/**
 * Represents a class definition in the abstract syntax tree.
 */
public class _Class {
  Token identifier;
  Token parentIdentifier;
  ClassBody body;

  protected _Class() {
  }

  public _Class(Token identifier, Token parentIdentifier, ClassBody body) {
    this.identifier = identifier;
    this.parentIdentifier = parentIdentifier;
    this.body = body;
  }

  public boolean hasParent() {
    return parentIdentifier != null;
  }
}

package scp.frontend.java.ast;

import scp.frontend.java.lexer.Token;

public class MethodAttributes {
  TypeSpecifier returnType;
  Token identifier;

  public MethodAttributes(TypeSpecifier returnType, Token identifier) {
    this.returnType = returnType;
    this.identifier = identifier;
  }
}

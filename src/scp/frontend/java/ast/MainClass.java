package scp.frontend.java.ast;

import scp.frontend.java.lexer.Token;

/**
 * An artifact of the MiniJava language, this object in the context of
 * the abstract syntax tree represents the first class defined in the
 * source file which has a constrained syntax -- it can only have a
 * public static void main function.
 */
public class MainClass extends _Class {
  Token classIdentifier;
  Token mainIdentifier;
  Token paramTypeIdentifier;
  Token paramIdentifier;
  Statement statement;

  /**
   * Constructs a new MainClass object based on the tokens from the
   * parse.  This is used directly in a parser action.
   */
  public MainClass(Token classIdentifier,
                   Token mainIdentifier,
                   Token paramTypeIdentifier,
                   Token paramIdentifier,
                   Statement statement) {
    this.classIdentifier = classIdentifier;
    this.mainIdentifier = mainIdentifier;
    this.paramTypeIdentifier = paramTypeIdentifier;
    this.paramIdentifier = paramIdentifier;
    this.statement = statement;
  }
}

package scp.frontend.java.ast;

/**
 * Represents a single source file in the abstract syntax tree.
 */
public class Program {
  ClassList classList;

  /**
   * Construct a Program node based on the list of classes that
   * occurred in the corresponding source file.
   */
  public Program(ClassList classList) {
    this.classList = classList;
  }
}

package scp.frontend.java.ast;

/**
 * Represents an array type specifier syntax.
 */
public class ArrayTypeSpecifier extends TypeSpecifier {
  TypeSpecifier elementType;

  /**
   * Constructs a new instance of this object with the specified
   * element type.
   */
  public ArrayTypeSpecifier(TypeSpecifier elementType) {
    this.elementType = elementType;
  }
}

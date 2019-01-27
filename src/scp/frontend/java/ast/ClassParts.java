package scp.frontend.java.ast;

/**
 * Fields and method declarations are all categorized as "class
 * parts".  This class is a collection of class parts.
 */
public class ClassParts {
  ClassPart[] classParts;
  int size;

  /**
   * Construct a new, empty ClassParts object.
   */
  public ClassParts() {
    this.classParts = new ClassPart[8];
    this.size = 0;
  }

  /**
   * Construct a new ClassParts object which contains a single
   * ClassPart.
   */
  public ClassParts(ClassPart classPart) {
    this.classParts = new ClassPart[8];
    this.classParts[0] = classPart;
    this.size = 1;
  }

  /**
   * Ensure that the capacity of the internal array is at least
   * newSize.
   */
  private void ensureCapacity(int newSize) {
    if (newSize <= this.classParts.length)
      return;

    ClassPart[] newClassParts = new ClassPart[this.size + newSize];
    for (int i = 0; i < size; i++)
      newClassParts[i] = this.classParts[i];

    this.classParts = newClassParts;
  }

  /**
   * Add the specified ClassPart object to the collection.
   */
  public ClassParts append(ClassPart part) {
    ensureCapacity(this.size + 1);
    this.classParts[this.size] = part;
    this.size++;
    return this;
  }
}

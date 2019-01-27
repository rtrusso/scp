package scp.frontend.java.ast;

/**
 * Represents a list of class definitions in the abstract syntax tree.
 */
public class ClassList {
  _Class[] classes;
  int size;

  private void ensureCapacity(int capacity) {
    if (this.classes.length < capacity) {
      _Class[] newClasses = new _Class[capacity + this.classes.length];
      for (int i = 0; i < size; i++)
        newClasses[i] = this.classes[i];

      this.classes = newClasses;
    }
  }

  public ClassList(_Class c) {
    classes = new _Class[16];
    classes[0] = c;
    size = 1;
  }

  public ClassList append(_Class c) {
    ensureCapacity(size + 1);
    this.classes[size] = c;
    this.size++;
    return this;
  }

  public int size() {
    return this.size;
  }

  public _Class at(int i) {
    if (i < size) {
      return this.classes[i];
    } else {
      throw new RuntimeException();
    }
  }
}

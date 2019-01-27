package scp.frontend.java.ast;

/**
 * A list of parameters to a method.
 */
public class ParameterList {
  Parameter[] parameters;
  int size;

  /**
   * Construct a new instance which is an empty list of parameters.
   */
  public ParameterList() {
    this.parameters = new Parameter[8];
    this.size = 0;
  }

  /**
   * Construct a new instance which contains only the specified parameter.
   */
  public ParameterList(Parameter parameter) {
    this.parameters = new Parameter[8];
    this.parameters[0] = parameter;
    this.size = 1;
  }

  /**
   * Called internally to ensure there is sufficient space to perform
   * an append operation.
   */
  private void ensureCapacity(int capacity) {
    if (capacity <= this.parameters.length)
      return;

    Parameter[] newParameters = new Parameter[this.size + capacity];
    for (int i = 0; i < size; i++)
      newParameters[i] = this.parameters[i];

    this.parameters = newParameters;
  }

  /**
   * Append the specified parameter to the parameter list.
   */
  public ParameterList append(Parameter parameter) {
    ensureCapacity(this.size + 1);
    this.parameters[this.size] = parameter;
    this.size++;
    return this;
  }
}

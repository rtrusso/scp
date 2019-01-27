package scp.frontend.java.ast;

/**
 * Represents the syntactic definition of a method within a class.
 */
public class MethodDefinition extends ClassPart {
  MethodAttributes attributes;
  ParameterList parameters;
  MethodBody body;

  /**
   * Construct a new instance specifying the attributes, parameters
   * and body of the method definition.
   */
  public MethodDefinition(MethodAttributes attributes, ParameterList parameters, MethodBody body) {
    this.attributes = attributes;
    this.parameters = parameters;
    this.body = body;
  }
}

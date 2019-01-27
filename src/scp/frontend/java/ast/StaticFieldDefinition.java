package scp.frontend.java.ast;

/**
 * Represents a static field definition in a class.
 */
public class StaticFieldDefinition extends FieldDefinition {

  /**
   * Constructs a new instance based on the specified variable
   * definition syntactic object.
   */
  public StaticFieldDefinition(VariableDefinition variableDefinition) {
    super(variableDefinition, true);
  }
}

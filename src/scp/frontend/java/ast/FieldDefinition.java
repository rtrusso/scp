package scp.frontend.java.ast;

/**
 * Represents a field definition in a class.
 */
public class FieldDefinition extends ClassPart {
  VariableDefinition variableDefinition;
  boolean isStatic = false;

  /**
   * Constructs a new instance based on the specified variable
   * definition syntactic object.
   */
  public FieldDefinition(VariableDefinition variableDefinition) {
    this.variableDefinition = variableDefinition;
  }

  /**
   * Constructs a new instance based on the specified variable
   * definition syntactic object and a flag which determines whether
   * the field is static or not.  This is intended to be used only by
   * StaticFieldDefinition.
   */
  protected FieldDefinition(VariableDefinition variableDefinition, boolean isStatic) {
    this.variableDefinition = variableDefinition;
    this.isStatic = isStatic;
  }

  public boolean isStatic() {
    return isStatic;
  }
}

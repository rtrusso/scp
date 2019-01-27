package scp.frontend.java.ast;

/**
 * Represents the syntactic structure of a method body.
 */
public class MethodBody {
  VariableDefinitionBlock variables;
  StatementBlock statements;

  public MethodBody(VariableDefinitionBlock variables, StatementBlock statements) {
    this.variables = variables;
    this.statements = statements;
  }
}

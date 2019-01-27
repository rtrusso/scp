package scp.frontend.java.ast;

public class VariableDefinitionBlock {
  VariableDefinition[] variables;
  int size;

  public VariableDefinitionBlock() {
    this.variables = new VariableDefinition[8];
    this.size = 0;
  }

  public VariableDefinitionBlock(VariableDefinition v) {
    this.variables = new VariableDefinition[8];
    this.variables[0] = v;
    this.size = 1;
  }

  private void ensureCapacity(int capacity) {
    if (capacity <= this.variables.length)
      return;

    VariableDefinition[] newVariables = new VariableDefinition[this.size + capacity];
    for (int i = 0; i < size; i++)
      newVariables[i] = this.variables[i];

    this.variables = newVariables;
  }

  public VariableDefinitionBlock append(VariableDefinition v) {
    ensureCapacity(this.size + 1);
    this.variables[this.size] = v;
    this.size++;
    return this;
  }
}

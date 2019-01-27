package scp.frontend.java.ast;

public class StatementBlock {
  Statement[] statements;
  int size;

  public StatementBlock() {
    this.statements = new Statement[8];
    this.size = 0;
  }

  public StatementBlock(Statement s) {
    this.statements = new Statement[8];
    this.statements[0] = s;
    this.size = 1;
  }

  private void ensureCapacity(int capacity) {
    if (capacity <= this.statements.length)
      return;

    Statement[] newStatements = new Statement[this.size + capacity];
    for (int i = 0; i < size; i++)
      newStatements[i] = this.statements[i];

    this.statements = newStatements;
  }

  public StatementBlock append(Statement s) {
    ensureCapacity(this.size + 1);
    this.statements[this.size] = s;
    this.size++;
    return this;
  }
}

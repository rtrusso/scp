package scp.frontend.java.ast;

public interface Visitor {
  void visitClass(_Class _class);
  void visitProgram(Program program);
  void visitStatement(Statement statement);
}

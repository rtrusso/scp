package scp.frontend.java.ast;

public interface Node {
  void applyVisitor(Visitor v);
}

package scp.frontend.java.lexer;

public class Lexer {
    private AdjacencyRule[] adjacencyRules;
    private int currentState;
    private AdjacencyRule currentRule;
    private StringBuffer currentValue;
    private int line;
    private int column;
    private int startLine;
    private int startColumn;

    public Lexer(int line, int column) {
        this.currentState = JavaLexerData.startState;
        this.adjacencyRules = JavaLexerData.getAdjacencyRules();
        this.currentRule = this.adjacencyRules[this.currentState];
        this.currentValue = new StringBuffer();
        this.line = line;
        this.column = column;
        this.startLine = line;
        this.startColumn = column;
    }

    public boolean empty() {
        return this.currentValue.length() == 0;
    }

    public boolean validState() {
        return currentState >= 0;
    }

    public boolean endState() {
        if (validState() == false)
            return true;

        return JavaLexerData.endStateMap[currentState];
    }

    public boolean hasToken() {
        if (validState() == false || endState() == false)
            return false;

        return JavaLexerData.emitMap[currentState] != null;
    }

    public Token currentToken() {
        if (hasToken() == false)
            return null;

        return new Token(currentValue.toString(),
                         JavaLexerData.emitMap[currentState],
                         startLine,
                         startColumn);
    }

    public boolean canConsume(char c) {
        int nextState = currentRule.lookup(c);

        return nextState >= 0;
    }

    public void consume(char c) {
        if (validState() == false)
            return;

        int nextState = currentRule.lookup(c);
        if (nextState >= 0) {
            currentRule = adjacencyRules[nextState];
            currentValue.append(c);
            if (c == '\n') {
                line++;
                column = 1;
            } else {
                column++;
            }
        }

        this.currentState = nextState;
    }

    public int getLine() { return line; }
    public int getColumn() { return column; }

    public void startNextToken() {
        currentState = JavaLexerData.startState;
        currentRule = adjacencyRules[currentState];
        currentValue.delete(0, currentValue.length());
        startLine = line;
        startColumn = column;
    }
}

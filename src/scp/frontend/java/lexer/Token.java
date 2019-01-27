package scp.frontend.java.lexer;

public class Token {
    private String value;
    private String type;
    private TokenType resolvedType;
    private int line;
    private int column;

    public Token(String value, String type, int line, int column) {
        this.value = value;
        this.type = type;
        this.line = line;
        this.column = column;
        this.resolvedType = TokenType.resolve(type);
    }

    public String getValue() { return value; }
    public String getTypeName() { return type; }
    public TokenType getType() { return resolvedType; }
    public int getLine() { return line; }
    public int getColumn() { return column; }
}

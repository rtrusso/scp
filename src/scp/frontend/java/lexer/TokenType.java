package scp.frontend.java.lexer;

public class TokenType {
    private static int counter = 0;
    private static TokenType[] allTypes = new TokenType[100];
    private String name;
    private int index;

    private TokenType(String name) {
        allTypes[counter] = this;
        this.name = name;
        this.index = ++counter;
    }

    public int code() {
        return index;
    }

    public static TokenType resolve(String typeName) {
        for (int i = 0; i < allTypes.length; i++) {
            if (allTypes[i] == null)
                break;

            if (allTypes[i].name.equals(typeName))
                return allTypes[i];
        }

        return null;
    }

    public static TokenType number = new TokenType("number");
    public static TokenType ident = new TokenType("ident");
    public static TokenType stringLiteral = new TokenType("string-literal");
    public static TokenType charLiteral = new TokenType("char-literal");
    public static TokenType comma = new TokenType("comma");
    public static TokenType bang = new TokenType("bang");
    public static TokenType systemOutPrintln = new TokenType("system-out-println");
    public static TokenType fail = new TokenType("fail");
    public static TokenType falseKeyword = new TokenType("false");
    public static TokenType thisKeyword = new TokenType("this");
    public static TokenType trueKeyword = new TokenType("true");
    public static TokenType carat = new TokenType("^");
    public static TokenType tilde = new TokenType("~");
    public static TokenType pipe = new TokenType("pipe");
    public static TokenType questionMark = new TokenType("?");
    public static TokenType dot = new TokenType("dot");
    public static TokenType asterisk = new TokenType("*");
    public static TokenType dash = new TokenType("-");
    public static TokenType plus = new TokenType("+");
    public static TokenType arithmeticShiftRight = new TokenType(">>>");
    public static TokenType shiftRight = new TokenType(">>");
    public static TokenType shiftLeft = new TokenType("<<");
    public static TokenType greaterThan = new TokenType(">");
    public static TokenType lessThan = new TokenType("<");
    public static TokenType logicalAnd = new TokenType("&&");
    public static TokenType bitwiseAnd = new TokenType("&");
    public static TokenType equality = new TokenType("==");
    public static TokenType assignment = new TokenType("=");
    public static TokenType whileKeyword = new TokenType("while");
    public static TokenType returnKeyword = new TokenType("return");
    public static TokenType booleanKeyword = new TokenType("boolean");
    public static TokenType ifKeyword = new TokenType("if");
    public static TokenType intKeyword = new TokenType("int");
    public static TokenType semiColon = new TokenType("s-colon");
    public static TokenType rightBracket = new TokenType("r-brak");
    public static TokenType leftBracket = new TokenType("l-brak");
    public static TokenType voidKeyword = new TokenType("void");
    public static TokenType publicKeyword = new TokenType("public");
    public static TokenType rightCurly = new TokenType("r-curl");
    public static TokenType leftCurly = new TokenType("l-curl");
    public static TokenType rightParen = new TokenType("r-paren");
    public static TokenType leftParen = new TokenType("l-paren");
    public static TokenType newKeyword = new TokenType("new");
    public static TokenType elseKeyword = new TokenType("else");
    public static TokenType extendsKeyword = new TokenType("extends");
    public static TokenType lengthKeyword = new TokenType("length");
    public static TokenType staticKeyword = new TokenType("static");
    public static TokenType superKeyword = new TokenType("super");
    public static TokenType charKeyword = new TokenType("char");
    public static TokenType classKeyword = new TokenType("class");
    public static TokenType sasmImplKeyword = new TokenType("--sasm-impl");
    public static TokenType comment = new TokenType("*comment*");
    public static TokenType whitespace = new TokenType("*whitespace*");
}

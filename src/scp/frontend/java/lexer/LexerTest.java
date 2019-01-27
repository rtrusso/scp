package scp.frontend.java.lexer;

import java.io.*;

public class LexerTest {

    private static void printValue(String value) {
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);

            switch (c) {
            case '\r':
                System.out.print("\\r");
                break;

            case '\n':
                System.out.print("\\n");
                break;

            case '\t':
                System.out.print("\\t");
                break;
                
            case '\\':
                System.out.print("\\\\");
                break;

            default:
                System.out.print(c);
                break;
            }
        }
    }

    private static void printToken(Token t) {
        TokenType type = t.getType();

        System.out.print(t.getTypeName() + ":" + (type == null ? -1 : type.code()) + "(");
        printValue(t.getValue());
        System.out.println(") " + t.getLine() + ", " + t.getColumn());
    }

    private static void scannerError(Lexer l) {
        System.out.println("Scanner error at line " + l.getLine() + ", " + l.getColumn());
        System.exit(1);
    }

    public static void main(String[] args) throws Exception {
        for (int i = 0; i < args.length; i++) {
            String file = args[i];
            FileInputStream fis = new FileInputStream(file);
            InputStreamReader isr = new InputStreamReader(fis);
            Lexer l = new Lexer(1, 1);
            int c;

            System.out.println("file [" + file + "]");
            while ((c = isr.read()) >= 0) {
                if (l.canConsume((char)c)) {
                    l.consume((char)c);
                } else if (l.hasToken()) {
                    Token t = l.currentToken();

                    printToken(t);
                    l.startNextToken();
                    if (l.canConsume((char)c))
                        l.consume((char)c);
                    else
                        scannerError(l);
                } else {
                    scannerError(l);
                }
            }

            if (l.hasToken() == true) {
                printToken(l.currentToken());
            } else if (l.empty() == false) {
                scannerError(l);
            }

            System.out.println("\n\n");
        }
    }
}

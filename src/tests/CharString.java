class CharString
{
  public static void main( String[] args ) {
    System.out.println( new Test().Go() );
  }
}

class Test
{
  public int Go()
  {
    int i;

    i = this.Test1();
    i = i + this.Test2();

    return i;
  }

  public int Test1() {
    String s;
    char[] c;

    c = new char[4];
    c[0] = 'a';
    c[1] = 'b';
    c[2] = 'c';
    c[3] = '\0';
    s = new String( c );

    System.out.println( s.length() );
    return s.length();
  }

  public int Test2() {
    String s;
    s = "asdf\0";

    System.out.println( s.length() );
    return s.length();
  }
}

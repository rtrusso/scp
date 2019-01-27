class CtorTest {
  public static void main( String[] args ) {
    System.out.println( new Test().Go() );
  }
}

class A {
  public A() {
    System.out.println( 1 );
    return;
  }
}

class Test extends A {
  public Test() {
    System.out.println( 2 );
  }

  public int Go() {
    return 1;
  }
}

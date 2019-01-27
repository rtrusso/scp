class OverrideTest {
  public static void main( String[] args ) {
    System.out.println( new Test2().Go() );
  }
}

class Test
{
  public int Go() {
    return 1;
  }
  public int Go( int i ) {
    return i + 1;
  }
}

class Test2 extends Test
{
  public int Go2() {
    return 2;
  }
  public int Go( int i  ) {
    return i + 2;
  }
  public int Go2( int i ) {
    return 3 + i;
  }
}

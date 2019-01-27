class StaticMethods
{
  public static void main( String[] args )
  {
    System.out.println( A.Test() );
  }
}

class A
{
  public static int Foo( int a, int b )
  {
    return a + b;
  }

  public static int Test()
  {
    return A.Foo( 1, 2 );
  }
}

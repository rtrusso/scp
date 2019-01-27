class GCTest1 {
  public static void main( String[] args ) {
    System.out.println( new Test().Go() );
  }
}

class Test
{
  public int Go() {
    int[] a;

    a = new int[64000];
    System.out.println( a.length );
    a = new int[1];
    System.out.println( a.length );
    a = new int[64000];
    System.out.println( a.length );
    a = new int[64000];
    System.out.println( a.length );
    a = new int[64000];
    System.out.println( a.length );
    a = new int[64000];
    System.out.println( a.length );
    a = new int[64000];
    System.out.println( a.length );

    return a.length;
  }
}

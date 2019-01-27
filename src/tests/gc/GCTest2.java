class GCTest2 {
  public static void main( String[] args ) {
    System.out.println( new Test().Go() );
  }
}

class Test
{
  public int Go() {
    int[] a;
    int index;

    index = 0;
    while (index < 50000)
    {
        a = new int[32000];
        System.out.println( a.length );

        a = new int[1];
        System.out.println( a.length );

        a = new int[32000];
        System.out.println( a.length );

        a = new int[1];
        System.out.println( a.length );

        a = new int[32000];
        System.out.println( a.length );

        a = new int[1];
        System.out.println( a.length );

        a = new int[32000];
        System.out.println( a.length );

        index = index + 1;
    }

    return a.length;
  }
}

class Bitwise {
  public static void main( String[] args ) {
    System.out.println( new Test().Go() );
  }
}

class Test {

  public int Go() {
    int waste;
    int zero;

    zero = 0;
    System.out.println( 99991 );
    waste = this.Single( 0, 0 );
    System.out.println( 99992 );
    waste = this.Single( 0, 1 );
    System.out.println( 99993 );
    waste = this.Single( 1, 1 );
    System.out.println( 99994 );
    waste = this.Single( 32, 3 );
    System.out.println( 99995 );
    waste = this.Single( 3, 3 );
    System.out.println( 99996 );
    waste = this.Single( 5, 1 );
    System.out.println( 99997 );
    waste = this.Single( zero-1, 1 );
    System.out.println( 99998 );
    waste = this.Single( zero-3, 1 );
    System.out.println( 99999 );
    waste = this.Single( 15, 5 );
    System.out.println( 999910 );
    waste = this.Single( 7, 2 );
    System.out.println( 999911 );
    waste = this.Single( 8, 3 );
    System.out.println( 999912 );
    waste = this.Single( 9, 5 );
    System.out.println( 999913 );
    waste = this.Single( 2147483647, 32 );
    System.out.println( 999914 );
    waste = this.Single( 2147483647, 2147483647 );

    return 0;
  }

  public int Single( int x, int y ) {
    System.out.println( x );
    System.out.println( y );
    System.out.println( x >> y );
    System.out.println( x >>> y );
    System.out.println( x << y );
    System.out.println( x & y );
    System.out.println( x | y );
    System.out.println( x ^ y );
    System.out.println( ~x );
    System.out.println( ~y );
    return 0;
  }

}

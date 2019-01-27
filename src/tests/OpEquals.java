class OpEquals {
  public static void main( String[] args ) {
    System.out.println( new Test().Go() );
  }
}

class Test
{
  public int Go() {
    int ret;
    Test inst;

    ret = 0;

    if( 'a' == 32 ) {
      ret = ret + 1;
    } else { }

    if( 'a' == 'a' ) {
      ret = ret + 10;
    } else { }

    if( 'b' == 'a' ) {
      ret = ret + 100;
    } else { }

    if( 0 == '\0' ) {
      ret = ret + 1000;
    } else { }

    if( this == new Test() ) {
      ret = ret + 10000;
    } else { }

    inst = this;
    if( inst == this ) {
      ret = ret + 100000;
    } else { }

    return ret;
  }
}

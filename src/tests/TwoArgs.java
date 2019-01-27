class TwoArgs {
  public static void main(String[] args) {
    System.out.println(new Foo().Bar());
  }
}

/***asdf`````******/
/***/
/**/
/* asdf *asdf* */

class Foo
{
  public int Bar()
  {
    return this.TwoArgs( 1 - 3, 8 * this.GetVal() );
  }
  public int GetVal()
  {
    return 8;
  }
  public int TwoArgs( int a, int b )
  {
    System.out.println(a);
    System.out.println(b);
    return b;
  }
}

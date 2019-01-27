class test1 {
  public static void Main( string[] args ) {
    System.Console.WriteLine( new Test2().Go() );
  }
}

class Test
{
  virtual public int GoHelper() {
    return 3;
  }

  virtual public int Go() {
    return 1 + this.GoHelper();
  }
}

class Test2 : Test
{
  override public int GoHelper() {
    return 2;
  }
}
